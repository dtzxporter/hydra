use std::io::ErrorKind;
use std::marker::PhantomData;
use std::net::SocketAddr;

#[cfg(feature = "native-tls")]
use std::sync::Arc;

use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::net::TcpListener;

use tokio_tungstenite::accept_hdr_async_with_config;
use tokio_tungstenite::tungstenite;

use tungstenite::handshake::server::ErrorResponse;
use tungstenite::protocol::WebSocketConfig;

#[cfg(feature = "native-tls")]
use tokio_native_tls::TlsAcceptor;

use hydra::ChildSpec;
use hydra::ExitReason;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Pid;
use hydra::Process;
use hydra::ProcessFlags;

use crate::WebsocketRequest;
use crate::WebsocketResponse;

use crate::start_websocket_handler;
use crate::WebsocketHandler;
use crate::WebsocketServerConfig;

/// A [Process] that listens for websocket connections and spawns [WebsocketHandler]'s to read/write to them.
pub struct WebsocketServer<T: WebsocketHandler + Send + 'static> {
    config: WebsocketServerConfig,
    server: Option<Pid>,
    _handler: PhantomData<T>,
}

impl<T> WebsocketServer<T>
where
    T: WebsocketHandler + Send + 'static,
{
    /// Constructs a new instance of [WebsocketServer] with the given `config`.
    pub fn new(config: WebsocketServerConfig) -> Self {
        WebsocketServer {
            config,
            server: None,
            _handler: PhantomData,
        }
    }
}

impl<T> WebsocketServer<T>
where
    T: WebsocketHandler + Send + Sync + 'static,
{
    pub fn child_spec(self) -> ChildSpec {
        ChildSpec::new("WebsocketServer").start(move || {
            WebsocketServer::start_link(
                WebsocketServer {
                    config: self.config.clone(),
                    server: None,
                    _handler: PhantomData::<T>,
                },
                GenServerOptions::new(),
            )
        })
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
            self.config.clone(),
        )));

        Ok(())
    }

    async fn terminate(&mut self, _reason: ExitReason) {
        if let Some(server) = self.server.take() {
            Process::exit(server, ExitReason::Kill);
        }
    }
}

/// Internal [WebsocketServer] accept routine.
async fn server_accept<T, S>(stream: S, address: SocketAddr, config: WebsocketServerConfig)
where
    T: WebsocketHandler + Send + 'static,
    S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
{
    let mut handler: Option<T> = None;

    let callback = |request: &WebsocketRequest, response: WebsocketResponse| match T::accept(
        address, request, response,
    ) {
        Ok((response, rhandler)) => {
            handler = Some(rhandler);
            Ok(response)
        }
        Err(reason) => Err(ErrorResponse::new(Some(format!("{:?}", reason)))),
    };

    let ws_config = WebSocketConfig {
        max_message_size: config.max_message_size,
        max_frame_size: config.max_frame_size,
        ..Default::default()
    };

    let accept = match config.handshake_timeout {
        Some(timeout) => {
            Process::timeout(
                timeout,
                accept_hdr_async_with_config(stream, callback, Some(ws_config)),
            )
            .await
        }
        None => Ok(accept_hdr_async_with_config(stream, callback, Some(ws_config)).await),
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

    Process::spawn(start_websocket_handler(handler, stream));
}

/// Internal [WebsocketHandler] tls routine.
#[cfg(feature = "native-tls")]
async fn server_accept_tls<T, S>(
    tls: Arc<TlsAcceptor>,
    stream: S,
    address: SocketAddr,
    mut config: WebsocketServerConfig,
) where
    T: WebsocketHandler + Send + 'static,
    S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
{
    let started = std::time::Instant::now();

    let accept = match config.handshake_timeout {
        Some(timeout) => Process::timeout(timeout, tls.accept(stream)).await,
        None => Ok(tls.accept(stream).await),
    };

    if let Some(timeout) = config.handshake_timeout.as_mut() {
        *timeout -= started.elapsed().min(*timeout)
    }

    match accept {
        Ok(Ok(stream)) => server_accept::<T, _>(stream, address, config).await,
        Ok(Err(error)) => {
            #[cfg(feature = "tracing")]
            tracing::error!(error = ?error, address = ?address, "Failed to accept tls connection");

            #[cfg(not(feature = "tracing"))]
            let _ = error;
        }
        Err(_) => {
            #[cfg(feature = "tracing")]
            tracing::error!(timeout = ?config.handshake_timeout, address = ?address, "Websocket accept tls timeout");
        }
    };
}

/// Internal [WebsocketServer] process routine.
async fn server_process<T>(server: TcpListener, config: WebsocketServerConfig)
where
    T: WebsocketHandler + Send + 'static,
{
    #[cfg(feature = "native-tls")]
    let tls = {
        use std::sync::Arc;

        use tokio_native_tls::native_tls::Identity;
        use tokio_native_tls::native_tls::TlsAcceptor as NTlsAcceptor;
        use tokio_native_tls::TlsAcceptor;

        let identity = if let (Some(der), Some(password)) =
            (&config.tls_pkcs12_der, &config.tls_pkcs12_password)
        {
            Identity::from_pkcs12(der, password.as_str())
                .expect("Failed to parse pkcs12 certificate!")
        } else if let (Some(pem), Some(key)) = (&config.tls_pkcs8_pem, &config.tls_pkcs8_key) {
            Identity::from_pkcs8(pem, key).expect("Failed to parse pkcs8 certificate!")
        } else {
            panic!("Feature 'native-tls' enabled without providing a tls certificate!");
        };

        let tls = NTlsAcceptor::new(identity).expect("Failed to load identity!");

        Arc::new(TlsAcceptor::from(tls))
    };

    #[cfg(feature = "tracing")]
    tracing::info!(address = ?config.address, "Websocket server accepting connections");

    loop {
        match server.accept().await {
            Ok((stream, address)) => {
                #[cfg(feature = "tracing")]
                tracing::trace!(address = ?address, "Accepted socket connection");

                #[cfg(feature = "native-tls")]
                Process::spawn(server_accept_tls::<T, _>(
                    tls.clone(),
                    stream,
                    address,
                    config.clone(),
                ));

                #[cfg(not(feature = "native-tls"))]
                Process::spawn(server_accept::<T, _>(stream, address, config.clone()));
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
