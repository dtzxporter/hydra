use std::future::Future;
use std::time::Duration;

use tokio::sync::oneshot;

use serde::Deserialize;
use serde::Serialize;

use crate::CallError;
use crate::ChildSpec;
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
    Call(From, T),
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
///
/// ## Example
/// Let's start with a code example and then explore the available callbacks. Imagine we want to implement a service with a GenServer that works like a stack, allowing us to push and pop elements. We'll customize a generic GenServer with our own module by implementing three callbacks.
///
/// ```no_run
/// #[derive(Debug, Serialize, Deserialize)]
/// enum StackMessage {
///     Pop,
///     PopResult(String),
///     Push(String),
/// }
///
/// struct Stack {
///     stack: Vec<String>,
/// }
///
/// impl Stack {
///     pub fn with_entries(entries: Vec<&'static str>) -> Self {
///         Self {
///             stack: Vec::from_iter(entries.into_iter().map(Into::into)),
///         }
///     }
/// }
///
/// impl GenServer for Stack {
///     type Message = StackMessage;
///
///     async fn init(&mut self) -> Result<(), ExitReason> {
///         Ok(())
///     }
///
///     async fn handle_call(&mut self, message: Self::Message, _from: From) -> Result<Option<Self::Message>, ExitReason> {
///         match message {
///             StackMessage::Pop => Ok(Some(StackMessage::PopResult(self.stack.remove(0)))),
///             _ => unreachable!(),
///         }
///     }
///
///     async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
///         match message {
///             StackMessage::Push(value) => self.stack.insert(0, value),
///             _ => unreachable!(),
///         }
///         Ok(())
///     }
/// }
/// ```
///
/// We leave the process machinery of startup, message passing, and the message loop to the GenServer.
/// We can now use the GenServer methods to interact with the service by creating a process and sending it messages:
/// ```no_run
/// // Start the server.
/// let pid = Stack::with_entries(vec![String::from("hello"), String::from("world")])
///             .start_link(GenServerOptions::new())
///             .await
///             .expect("Failed to start stack!");
///
/// // This is the client.
/// Stack::call(pid, StackMessage::Pop, None)
///         .await
///         .expect("Stack call failed!");
/// // => StackMessage::PopResult("hello")
///
/// Stack::cast(pid, StackMessage::Push(String::from("rust")))
///
/// Stack::call(pid, StackMessage::Pop, None)
///         .await
///         .expect("Stack call failed!");
/// // => StackMessage::PopResult("rust")
/// ```
pub trait GenServer: Sized + Send + 'static {
    /// The message type that this server will use.
    type Message: Receivable;

    /// Invoked when the server is started. `start_link` or `start` will block until it returns.
    fn init(&mut self) -> impl Future<Output = Result<(), ExitReason>> + Send;

    /// Starts a [GenServer] process without links.
    fn start(
        self,
        options: GenServerOptions,
    ) -> impl Future<Output = Result<Pid, ExitReason>> + Send {
        async { start_gen_server(self, options, false).await }
    }

    /// Starts a [GenServer] process linked to the current process.
    fn start_link(
        self,

        options: GenServerOptions,
    ) -> impl Future<Output = Result<Pid, ExitReason>> + Send {
        async { start_gen_server(self, options, true).await }
    }

    /// Builds a child specification for this [GenServer] process.
    fn child_spec() -> ChildSpec {
        unimplemented!("User must implement child_spec")
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

    /// Casts a request to the `server` after the given `duration` without waiting for a response.
    ///
    /// It is unknown whether the destination server successfully handled the request.
    fn cast_after<T: Into<Dest>>(server: T, message: Self::Message, duration: Duration) {
        Process::send_after(server, GenServerMessage::Cast(message), duration);
    }

    /// Makes a synchronous call to the `server` and waits for it's reply.
    ///
    /// The client sends the given `message` to the server and waits until a reply
    /// arrives or a timeout occurs. `handle_call` will be called on the server to handle the request.
    ///
    /// The default timeout is 5000ms.
    fn call<T: Into<Dest>>(
        server: T,
        message: Self::Message,
        timeout: Option<Duration>,
    ) -> impl Future<Output = Result<Self::Message, CallError>> + Send {
        let server = server.into();

        async move {
            let monitor = if server.is_local() {
                Process::monitor(server.clone())
            } else {
                Process::monitor_alias(server.clone(), true)
            };

            let from = From::new(Process::current(), monitor, server.is_remote());

            Process::send(server, GenServerMessage::Call(from, message));

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
        if from.is_alias() {
            Process::send(from.tag(), GenServerMessage::CallReply(from.tag(), message));
        } else {
            Process::send(from.pid(), GenServerMessage::CallReply(from.tag(), message));
        }
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
            Process::timeout(duration, gen_server.init()).await
        } else {
            Ok(gen_server.init().await)
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
                Message::User(GenServerMessage::Call(from, message)) => {
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
