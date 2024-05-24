use std::net::SocketAddr;
use std::time::Duration;

#[derive(Debug, Clone, Copy)]
pub struct WebsocketServerConfig {
    pub(crate) address: SocketAddr,
    pub(crate) handshake_timeout: Option<Duration>,
}

impl WebsocketServerConfig {
    pub const fn new(address: SocketAddr) -> Self {
        Self {
            address,
            handshake_timeout: None,
        }
    }
}
