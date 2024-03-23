use std::future::Future;

use hydra::Dest;
use hydra::Message;
use hydra::Monitor;
use hydra::Pid;
use hydra::Process;

use hydra::SystemMessage;
use tokio::sync::oneshot;
use tokio::time::timeout;

use crate::GenServerOptions;
use crate::Reply;

pub(crate) enum GenServerMessage<T: Send + 'static> {
    Cast(Pid, T),
    Call(Pid, Monitor, T),
    CallReply(Monitor, T),
}

pub trait GenServer: Sized + Send + 'static {
    /// The type of the init argument that this server will use.
    type InitArg: Send;
    /// The message type that this server will use.
    type Message: Send + 'static;

    fn init(&mut self, init_arg: Self::InitArg) -> impl Future<Output = ()> + Send;

    async fn start(self, init_arg: Self::InitArg, options: GenServerOptions) -> Pid {
        start_internal(self, init_arg, options, false).await
    }

    async fn start_link(self, init_arg: Self::InitArg, options: GenServerOptions) -> Pid {
        start_internal(self, init_arg, options, true).await
    }

    fn cast<T: Into<Dest>>(dest: T, message: Self::Message) {
        Process::send(dest, GenServerMessage::Cast(Process::current(), message));
    }

    async fn call<T: Into<Dest>>(dest: T, message: Self::Message) -> Self::Message {
        let dest = dest.into();
        let monitor = Process::monitor(dest.clone());

        Process::send(
            dest,
            GenServerMessage::Call(Process::current(), monitor, message),
        );

        let receiver = Process::receiver();

        loop {
            let message = receiver
                .filter_receive::<GenServerMessage<Self::Message>>()
                .await;

            match message {
                Message::User(GenServerMessage::CallReply(mref, reply)) => {
                    if monitor == mref {
                        Process::demonitor(monitor);
                        return reply;
                    } else {
                        receiver.keep(Message::User(GenServerMessage::CallReply(mref, reply)));
                    }
                }
                Message::System(SystemMessage::ProcessDown(_, mref, ref exit_reason)) => {
                    if monitor == mref {
                        panic!(
                            "Process went down while waiting for a reply: {:?}",
                            exit_reason
                        );
                    } else {
                        receiver.keep(message);
                    }
                }
                _ => receiver.keep(message),
            }
        }
    }

    fn handle_info(&mut self, info: Message<Self::Message>) -> impl Future<Output = ()> + Send {
        async move {
            let _ = info;
        }
    }

    /// A method.
    fn handle_cast(
        &mut self,
        from: Pid,
        message: Self::Message,
    ) -> impl Future<Output = ()> + Send {
        async move {
            let _ = message;

            println!(
                "GenServer: {:?} received unhandled cast from {:?}",
                Process::current(),
                from
            );
        }
    }

    /// A call.
    fn handle_call(
        &mut self,
        reply: Reply<Self::Message>,
        message: Self::Message,
    ) -> impl Future<Output = Option<Self::Message>> + Send {
        async move {
            let _ = message;

            panic!(
                "GenServer: {:?} received unhandled call from {:?}",
                Process::current(),
                reply.to(),
            );
        }
    }
}

/// Internal gen server start utility.
async fn start_internal<T: GenServer + Send + 'static>(
    gen_server: T,
    init_arg: T::InitArg,
    mut options: GenServerOptions,
    link: bool,
) -> Pid {
    let (tx, rx) = oneshot::channel::<()>();

    let server = async move {
        let mut gen_server = gen_server;

        if let Some(duration) = options.timeout.take() {
            let _ = timeout(duration, gen_server.init(init_arg)).await;
        } else {
            gen_server.init(init_arg).await;
        }

        let _ = tx.send(());

        loop {
            let message = Process::receive::<GenServerMessage<T::Message>>().await;

            match message {
                Message::User(GenServerMessage::Cast(from, cast)) => {
                    gen_server.handle_cast(from, cast).await
                }
                Message::User(GenServerMessage::Call(from, mref, call)) => {
                    let reply = Reply::new(from, mref);

                    if let Some(reply) = gen_server.handle_call(reply, call).await {
                        Process::send(from, GenServerMessage::CallReply(mref, reply));
                    }
                }
                Message::System(system) => {
                    gen_server.handle_info(Message::System(system)).await;
                }
                _ => unimplemented!(),
            }
        }
    };

    let pid = if link {
        Process::spawn_link(server)
    } else {
        Process::spawn(server)
    };

    let _ = rx.await;

    pid
}
