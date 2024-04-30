use std::future::Future;
use std::time::Duration;

use tokio::sync::oneshot;

use serde::Deserialize;
use serde::Serialize;

use crate::CallError;
use crate::Dest;
use crate::ExitReason;
use crate::From;
use crate::GenServerOptions;
use crate::Message;
use crate::Pid;
use crate::Process;
use crate::Receivable;
use crate::Reference;
use crate::SystemMessage;

/// Unique message type for a [GenServer] cast, call, and reply.
#[derive(Debug, Serialize, Deserialize)]
enum GenServerMessage<T: Send + 'static> {
    #[serde(rename = "$gen_cast")]
    Cast(T),
    #[serde(rename = "$gen_call")]
    Call(Pid, Reference, T),
    #[serde(rename = "$gen_reply")]
    CallReply(Reference, T),
    #[serde(rename = "$gen_stop")]
    Stop(ExitReason),
}

/// A trait for implementing the server of a client-server relation.
///
/// A [GenServer] is a process like any other hydra process and it can be used to keep state,
/// execute code asynchronously and so on.
///
/// The advantage of using a generic server process (GenServer) implemented using this
/// trait is that it will have a standard set of trait functions and include functionality
/// for tracing and error reporting.
///
/// It will also fit into a supervision tree.
pub trait GenServer: Sized + Send + 'static {
    /// The type of the init argument that this server will use.
    type InitArg: Send;
    /// The message type that this server will use.
    type Message: Receivable;

    /// Invoked when the server is started. `start_link` or `start` will block until it returns.
    fn init(
        &mut self,
        init_arg: Self::InitArg,
    ) -> impl Future<Output = Result<(), ExitReason>> + Send;

    /// Starts a [GenServer] process without links.
    fn start(
        self,
        init_arg: Self::InitArg,
        options: GenServerOptions,
    ) -> impl Future<Output = Result<Pid, ExitReason>> + Send {
        async { start_gen_server(self, init_arg, options, false).await }
    }

    /// Starts a [GenServer] process linked to the current process.
    fn start_link(
        self,
        init_arg: Self::InitArg,
        options: GenServerOptions,
    ) -> impl Future<Output = Result<Pid, ExitReason>> + Send {
        async { start_gen_server(self, init_arg, options, true).await }
    }

    /// Synchronously stops the server with the given `reason`.
    ///
    /// The `terminate` callback of the given `server` will be invoked before exiting. This function returns an error if the process
    /// exits with a reason other than the given `reason`.
    ///
    /// The default timeout is infinity.
    fn stop<T: Into<Dest> + Send>(
        server: T,
        reason: ExitReason,
        timeout: Option<Duration>,
    ) -> impl Future<Output = Result<(), ExitReason>> {
        async move {
            let server = server.into();
            let monitor = Process::monitor(server.clone());

            Process::send(
                server,
                GenServerMessage::<Self::Message>::Stop(reason.clone()),
            );

            let receiver = Process::receiver()
                .ignore_type()
                .select::<GenServerMessage<Self::Message>, _>(|message| {
                    match message {
                        Message::System(SystemMessage::ProcessDown(_, tag, _)) => {
                            // Make sure the tag matches the monitor.
                            *tag == monitor
                        }
                        _ => false,
                    }
                });

            let result = match timeout {
                Some(duration) => Process::timeout(duration, receiver).await,
                None => Ok(receiver.await),
            };

            match result {
                Ok(Message::System(SystemMessage::ProcessDown(_, _, exit_reason))) => {
                    if reason == exit_reason {
                        Ok(())
                    } else {
                        Err(exit_reason)
                    }
                }
                Err(_) => {
                    Process::demonitor(monitor);

                    Err(ExitReason::from("timeout"))
                }
                _ => unreachable!(),
            }
        }
    }

    /// Casts a request to the `server` without waiting for a response.
    ///
    /// It is unknown whether the destination server successfully handled the request.
    fn cast<T: Into<Dest>>(server: T, message: Self::Message) {
        Process::send(server, GenServerMessage::Cast(message));
    }

    /// Makes a synchronous call to the `server` and waits for it's reply.
    ///
    /// The client sends the given `message` to the server and waits until a reply
    /// arrives or a timeout occurs. `handle_call` will be called on the server to handle the request.
    ///
    /// The default timeout is 5000ms.
    fn call<T: Into<Dest> + Send>(
        server: T,
        message: Self::Message,
        timeout: Option<Duration>,
    ) -> impl Future<Output = Result<Self::Message, CallError>> + Send {
        async move {
            let server = server.into();
            let monitor = Process::monitor(server.clone());

            // TODO: Determine if we want to use an alias here?
            // Use alias if the process is remote to prevent reply after down.

            Process::send(
                server,
                GenServerMessage::Call(Process::current(), monitor, message),
            );

            let receiver = Process::receiver()
                .ignore_type()
                .select::<GenServerMessage<Self::Message>, _>(|message| {
                    match message {
                        Message::User(GenServerMessage::CallReply(tag, _)) => {
                            // Make sure the tag matches the monitor.
                            *tag == monitor
                        }
                        Message::System(SystemMessage::ProcessDown(_, tag, _)) => {
                            // Make sure the tag matches the monitor.
                            *tag == monitor
                        }
                        _ => false,
                    }
                });

            let result =
                Process::timeout(timeout.unwrap_or(Duration::from_millis(5000)), receiver).await;

            match result {
                Ok(Message::User(GenServerMessage::CallReply(_, message))) => {
                    Process::demonitor(monitor);

                    Ok(message)
                }
                Ok(Message::System(SystemMessage::ProcessDown(_, _, reason))) => {
                    Err(CallError::ServerDown(reason))
                }
                Err(timeout) => {
                    Process::demonitor(monitor);

                    // Drop a stale reply that may already be in the process message inbox.
                    Process::receiver()
                        .ignore_type()
                        .drop::<GenServerMessage<Self::Message>, _>(|message| {
                            match message {
                                Message::User(GenServerMessage::CallReply(tag, _)) => {
                                    // Make sure the tag matches the monitor.
                                    *tag == monitor
                                }
                                _ => false,
                            }
                        });

                    Err(CallError::Timeout(timeout))
                }
                _ => unreachable!(),
            }
        }
    }

    /// Replies to a client.
    ///
    /// This function can be used to explicitly send a reply to a client that called `call` when the
    /// reply cannot be specified in the return value of `handle_call`.
    ///
    /// `client` must be the `from` argument accepted by `handle_call` callbacks.
    ///
    /// Note that `reply` can be called from any process, not just the [GenServer] that originally received the call
    /// (as long as the GenServer communicated the `from` argument somehow).
    fn reply(from: From, message: Self::Message) {
        Process::send(from.pid(), GenServerMessage::CallReply(from.tag(), message));
    }

    /// Invoked when the server is about to exit. It should do any cleanup required.
    ///
    /// `terminate` is useful for cleanup that requires access to the [GenServer]'s state. However, it is not
    /// guaranteed that `terminate` is called when a [GenServer] exits. Therefore, important cleanup should be done
    /// using process links and/or monitors. A monitoring process will receive the same `reason` that would be passed to `terminate`.
    ///
    /// `terminate` is called if:
    /// - The [GenServer] traps exits (using [Process::flags]) and the parent process sends an exit signal.
    /// - A callback (except `init`) returns stop with a given reason.
    fn terminate(&mut self, reason: ExitReason) -> impl Future<Output = ()> + Send {
        async move {
            let _ = reason;
        }
    }

    /// Invoked to handle asynchronous `cast` messages.
    fn handle_cast(
        &mut self,
        message: Self::Message,
    ) -> impl Future<Output = Result<(), ExitReason>> + Send {
        async move {
            let _ = message;

            unimplemented!();
        }
    }

    /// Invoked to handle all other messages.
    fn handle_info(
        &mut self,
        info: Message<Self::Message>,
    ) -> impl Future<Output = Result<(), ExitReason>> + Send {
        async move {
            let _ = info;

            Ok(())
        }
    }

    /// Invoked to handle synchronous `call` messages. `call` will block until a reply is received
    /// (unless the call times out or nodes are disconnected).
    ///
    /// `from` is a struct containing the callers [Pid] and a [Reference] that uniquely identifies the call.
    fn handle_call(
        &mut self,
        message: Self::Message,
        from: From,
    ) -> impl Future<Output = Result<Option<Self::Message>, ExitReason>> + Send {
        async move {
            let _ = message;
            let _ = from;

            unimplemented!();
        }
    }
}

/// Internal [GenServer] start routine.
async fn start_gen_server<T: GenServer>(
    gen_server: T,
    init_arg: T::InitArg,
    options: GenServerOptions,
    link: bool,
) -> Result<Pid, ExitReason> {
    let (tx, rx) = oneshot::channel::<Result<(), ExitReason>>();

    let server = async move {
        let mut gen_server = gen_server;
        let mut options = options;

        let registered = if let Some(name) = options.name.take() {
            Process::register(Process::current(), name).is_ok()
        } else {
            true
        };

        if !registered {
            tx.send(Err(ExitReason::from("already_started")))
                .expect("Failed to notify parent process!");
            return;
        }

        let timeout = if let Some(duration) = options.timeout.take() {
            Process::timeout(duration, gen_server.init(init_arg)).await
        } else {
            Ok(gen_server.init(init_arg).await)
        };

        match timeout {
            Ok(Ok(())) => {
                tx.send(Ok(())).expect("Failed to notify parent process!");
            }
            Ok(Err(reason)) => {
                tx.send(Err(reason))
                    .expect("Failed to notify parent process!");
                return;
            }
            Err(_) => {
                tx.send(Err(ExitReason::from("timeout")))
                    .expect("Failed to notify parent process!");
                return;
            }
        }

        loop {
            let message: Message<GenServerMessage<T::Message>> = Process::receive().await;

            match message {
                Message::User(GenServerMessage::Cast(message)) => {
                    if let Err(reason) = gen_server.handle_cast(message).await {
                        gen_server.terminate(reason.clone()).await;

                        return Process::exit(Process::current(), reason);
                    }
                }
                Message::User(GenServerMessage::Call(pid, tag, message)) => {
                    let from = From::new(pid, tag);

                    match gen_server.handle_call(message, from).await {
                        Ok(Some(message)) => {
                            T::reply(from, message);
                        }
                        Ok(None) => {
                            // Server must reply using `GenServer::reply(from, message)`.
                        }
                        Err(reason) => {
                            gen_server.terminate(reason.clone()).await;

                            return Process::exit(Process::current(), reason);
                        }
                    }
                }
                Message::User(GenServerMessage::CallReply(_, message)) => {
                    if let Err(reason) = gen_server.handle_info(Message::User(message)).await {
                        gen_server.terminate(reason.clone()).await;

                        return Process::exit(Process::current(), reason);
                    }
                }
                Message::User(GenServerMessage::Stop(reason)) => {
                    gen_server.terminate(reason.clone()).await;

                    return Process::exit(Process::current(), reason);
                }
                Message::System(system) => {
                    if let Err(reason) = gen_server.handle_info(Message::System(system)).await {
                        gen_server.terminate(reason.clone()).await;

                        return Process::exit(Process::current(), reason);
                    }
                }
            }
        }
    };

    let pid = if link {
        Process::spawn_link(server)
    } else {
        Process::spawn(server)
    };

    rx.await
        .map(|_| pid)
        .map_err(|_| ExitReason::from("unknown"))
}
