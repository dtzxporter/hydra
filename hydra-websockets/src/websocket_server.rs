use std::io::ErrorKind;
use std::marker::PhantomData;
use std::net::SocketAddr;

use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::net::TcpListener;

use tokio_tungstenite::accept_hdr_async;
use tokio_tungstenite::tungstenite;

use tungstenite::handshake::server::ErrorResponse;
use tungstenite::handshake::server::Request;
use tungstenite::handshake::server::Response;

use hydra::ExitReason;
use hydra::GenServer;
use hydra::Pid;
use hydra::Process;
use hydra::ProcessFlags;

use crate::start_websocket_handler;
use crate::WebsocketHandler;
use crate::WebsocketServerConfig;

pub struct WebsocketServer<T: WebsocketHandler + Send + 'static> {
    config: WebsocketServerConfig,
    server: Option<Pid>,
    _handler: PhantomData<T>,
}

impl<T> WebsocketServer<T>
where
    T: WebsocketHandler + Send + 'static,
{
    pub fn new(config: WebsocketServerConfig) -> Self {
        WebsocketServer {
            config,
            server: None,
            _handler: PhantomData,
        }
    }
}

impl<T> GenServer for WebsocketServer<T>
where
    T: WebsocketHandler + Send + 'static,
{
    type Message = ();

    async fn init(&mut self) -> Result<(), ExitReason> {
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        let server = match TcpListener::bind(self.config.address).await {
            Ok(server) => server,
            Err(error) => {
                let reason = match error.kind() {
                    ErrorKind::AddrInUse => "address_in_use",
                    ErrorKind::AddrNotAvailable => "address_not_available",
                    ErrorKind::WouldBlock => "would_block",
                    _ => "unknown",
                };

                return Err(ExitReason::from(reason));
            }
        };

        self.server = Some(Process::spawn_link(server_process::<T>(
            server,
            self.config,
        )));

        Ok(())
    }

    async fn terminate(&mut self, _reason: ExitReason) {
        if let Some(server) = self.server.take() {
            Process::exit(server, ExitReason::Kill);
        }
    }
}

async fn server_accept<T, S>(stream: S, address: SocketAddr, config: WebsocketServerConfig)
where
    T: WebsocketHandler + Send + 'static,
    S: AsyncRead + AsyncWrite + Unpin,
{
    let mut handler: Option<T> = None;

    let callback = |request: &Request, response: Response| match T::accept(request, response) {
        Ok((response, rhandler)) => {
            handler = Some(rhandler);
            Ok(response)
        }
        Err(reason) => Err(ErrorResponse::new(Some(format!("{:?}", reason)))),
    };

    let accept = match config.handshake_timeout {
        Some(timeout) => Process::timeout(timeout, accept_hdr_async(stream, callback)).await,
        None => Ok(accept_hdr_async(stream, callback).await),
    };

    let stream = match accept {
        Ok(Ok(stream)) => stream,
        Ok(Err(error)) => {
            #[cfg(feature = "tracing")]
            tracing::error!(error = ?error, address = ?address, "Failed to accept websocket");

            #[cfg(not(feature = "tracing"))]
            {
                let _ = error;
                let _ = address;
            }
            return;
        }
        Err(_) => {
            #[cfg(feature = "tracing")]
            tracing::error!(timeout = ?config.handshake_timeout, address = ?address, "Websocket accept timeout");
            return;
        }
    };

    let handler = handler
        .take()
        .expect("Must have a handler after accepting!");

    #[cfg(feature = "tracing")]
    tracing::trace!(address = ?address, "Accepted websocket connection");

    Process::spawn(start_websocket_handler(handler));
}

async fn server_process<T>(server: TcpListener, config: WebsocketServerConfig)
where
    T: WebsocketHandler + Send + 'static,
{
    loop {
        match server.accept().await {
            Ok((stream, address)) => {
                #[cfg(feature = "tracing")]
                tracing::trace!(address = ?address, "Accepted socket connection");

                Process::spawn(server_accept::<T, _>(stream, address, config));
            }
            Err(error) => {
                #[cfg(feature = "tracing")]
                tracing::error!(error = ?error, "Failed to accept connection");

                #[cfg(not(feature = "tracing"))]
                let _ = error;
            }
        }
    }
}
