use std::net::SocketAddr;
use std::time::Duration;

/// A set of configuration options for the websocket server.
#[derive(Debug, Clone, Copy)]
pub struct WebsocketServerConfig {
    pub(crate) address: SocketAddr,
    pub(crate) handshake_timeout: Option<Duration>,
}

impl WebsocketServerConfig {
    /// Constructs a new [WebsocketServerConfig] with the given address to listen on.
    pub const fn new(address: SocketAddr) -> Self {
        Self {
            address,
            handshake_timeout: None,
        }
    }

    /// Sets the timeout for websocket handshakes.
    pub const fn handshake_timeout(mut self, timeout: Duration) -> Self {
        self.handshake_timeout = Some(timeout);
        self
    }
}
