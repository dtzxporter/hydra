use std::future::Future;
use std::net::SocketAddr;

use futures_util::stream;
use futures_util::SinkExt;
use futures_util::StreamExt;

use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;

use tokio_tungstenite::tungstenite::protocol::CloseFrame;
use tokio_tungstenite::tungstenite::Error;
use tokio_tungstenite::WebSocketStream;

use hydra::ExitReason;
use hydra::Message;
use hydra::Process;
use hydra::Receivable;

use crate::WebsocketCommand;
use crate::WebsocketCommands;
use crate::WebsocketMessage;
use crate::WebsocketRequest;
use crate::WebsocketResponse;

/// A process that handles websocket messages.
pub trait WebsocketHandler
where
    Self: Sized,
{
    /// The message type that this handler will use.
    type Message: Receivable;

    /// A callback used to accept or deny a request for a websocket upgrade.
    ///
    /// You can extract information from the request and put it in your handler state.
    fn accept(
        address: SocketAddr,
        request: &WebsocketRequest,
        response: WebsocketResponse,
    ) -> Result<(WebsocketResponse, Self), ExitReason>;

    /// An optional callback that happens before the first message is sent/received from the websocket.
    ///
    /// This is the first callback that happens in the process responsible for the websocket.
    fn websocket_init(
        &mut self,
    ) -> impl Future<Output = Result<Option<WebsocketCommands>, ExitReason>> + Send {
        async move { Ok(None) }
    }

    /// Invoked to handle messages received from the websocket.
    fn websocket_handle(
        &mut self,
        message: WebsocketMessage,
    ) -> impl Future<Output = Result<Option<WebsocketCommands>, ExitReason>> + Send;

    /// Invoked to handle messages from processes and system messages.
    fn websocket_info(
        &mut self,
        info: Message<Self::Message>,
    ) -> impl Future<Output = Result<Option<WebsocketCommands>, ExitReason>> + Send {
        async move {
            let _ = info;

            Ok(None)
        }
    }

    /// Invoked when the handler is about to exit. It should do any cleanup required.
    ///
    /// `terminate` is useful for cleanup that requires access to the [WebsocketHandler]'s state. However, it is not
    /// guaranteed that `terminate` is called when a [WebsocketHandler] exits. Therefore, important cleanup should be done
    /// using process links and/or monitors. A monitoring process will receive the same `reason` that would be passed to `terminate`.
    ///
    /// `terminate` is called if:
    /// - The websocket connection closes for whatever reason.
    /// - A callback (except `accept`) returns stop with a given reason.
    fn terminate(&mut self, reason: ExitReason) -> impl Future<Output = ()> + Send {
        async move {
            let _ = reason;
        }
    }
}

/// Internal routine to process commands from a [WebsocketHandler] callback.
async fn websocket_process_commands<T, S>(
    commands: WebsocketCommands,
    handler: &mut T,
    stream: &mut WebSocketStream<S>,
) where
    T: WebsocketHandler + Send + 'static,
    S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
{
    let mut close_command: Option<WebsocketCommand> = None;

    let sends = commands.buffer.into_iter().filter_map(|command| {
        if close_command.is_some() {
            return None;
        }

        match command {
            WebsocketCommand::Send(message) => Some(Ok(message)),
            WebsocketCommand::Close(_, _) => {
                close_command = Some(command);
                None
            }
        }
    });

    let mut sends = stream::iter(sends);

    if let Err(error) = stream.send_all(&mut sends).await {
        handler.terminate(error_to_reason(&error)).await;

        Process::exit(Process::current(), error_to_reason(&error))
    }

    if let Some(WebsocketCommand::Close(code, reason)) = close_command {
        if let Err(error) = stream
            .close(Some(CloseFrame {
                code,
                reason: reason.into(),
            }))
            .await
        {
            handler.terminate(error_to_reason(&error)).await;

            Process::exit(Process::current(), error_to_reason(&error));
        } else {
            Process::exit(Process::current(), ExitReason::Normal);
        }
    }
}

/// Internal [WebsocketHandler] start routine.
pub(crate) async fn start_websocket_handler<T, S>(mut handler: T, mut stream: WebSocketStream<S>)
where
    T: WebsocketHandler + Send + 'static,
    S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
{
    match handler.websocket_init().await {
        Ok(commands) => {
            if let Some(commands) = commands {
                websocket_process_commands(commands, &mut handler, &mut stream).await;
            }
        }
        Err(reason) => {
            return Process::exit(Process::current(), reason);
        }
    }

    loop {
        tokio::select! {
            message = Process::receive::<T::Message>() => {
                match handler.websocket_info(message).await {
                    Ok(commands) => {
                        if let Some(commands) = commands {
                            websocket_process_commands(commands, &mut handler, &mut stream).await;
                        }
                    }
                    Err(reason) => {
                        handler.terminate(reason.clone()).await;

                        return Process::exit(Process::current(), reason);
                    }
                }
            }
            ws_message = stream.next() => {
                let Some(ws_message) = ws_message else {
                    panic!("Websocket closed without close frame!");
                };

                match ws_message {
                    Ok(message) => {
                        let mut should_close = false;

                        match &message {
                            WebsocketMessage::Ping(data) => {
                                if let Err(error) = stream.send(WebsocketMessage::Pong(data.clone())).await {
                                    handler.terminate(error_to_reason(&error)).await;

                                    return Process::exit(Process::current(), error_to_reason(&error));
                                }
                            }
                            WebsocketMessage::Close(_) => {
                                should_close = true;
                            }
                            _ => {
                                // No special handling.
                            }
                        }

                        match handler.websocket_handle(message).await {
                            Ok(commands) => {
                                if let Some(commands) = commands {
                                    websocket_process_commands(commands, &mut handler, &mut stream).await;
                                }
                            }
                            Err(reason) => {
                                handler.terminate(reason.clone()).await;

                                return Process::exit(Process::current(), reason);
                            }
                        }

                        if should_close {
                            handler.terminate(ExitReason::from("connection_closed")).await;

                            return Process::exit(Process::current(), ExitReason::from("connection_closed"));
                        }
                    }
                    Err(error) => {
                        handler.terminate(error_to_reason(&error)).await;

                        return Process::exit(Process::current(), error_to_reason(&error));
                    }
                }
            }
        }
    }
}

/// Converts a websocket error to an exit reason.
fn error_to_reason(error: &Error) -> ExitReason {
    match error {
        Error::AlreadyClosed | Error::ConnectionClosed => ExitReason::from("connection_closed"),
        Error::Io(_) => ExitReason::from("io_error"),
        Error::Tls(_) => ExitReason::from("tls_error"),
        Error::Utf8 => ExitReason::from("utf8_error"),
        Error::AttackAttempt => ExitReason::from("attack_attempt"),
        _ => ExitReason::from("unknown"),
    }
}
