use std::net::IpAddr;
use std::net::Ipv4Addr;
use std::net::SocketAddr;
use std::time::Duration;

/// Options used to configure this node as a distributed node.
#[derive(Clone, Copy)]
pub struct NodeOptions {
    pub(crate) listen_address: SocketAddr,
    pub(crate) broadcast_address: SocketAddr,
    pub(crate) handshake_timeout: Duration,
    pub(crate) heartbeat_interval: Duration,
    pub(crate) heartbeat_timeout: Duration,
}

impl NodeOptions {
    /// Constructs a new instance of [NodeOptions] with default values.
    pub const fn new() -> Self {
        Self {
            listen_address: SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 1337),
            broadcast_address: SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 1337),
            handshake_timeout: Duration::from_secs(5),
            heartbeat_interval: Duration::from_secs(5),
            heartbeat_timeout: Duration::from_secs(45),
        }
    }

    /// Sets the address this node will listen for incoming connections on.
    pub fn listen_address<T: Into<SocketAddr>>(mut self, address: T) -> Self {
        self.listen_address = address.into();
        self
    }

    /// Sets the address this node will advertise itself for remote connections.
    pub fn broadcast_address<T: Into<SocketAddr>>(mut self, address: T) -> Self {
        self.broadcast_address = address.into();
        self
    }

    /// Sets the time it takes to timeout a remote node handshake. (Default 5s)
    pub fn handshake_timeout(mut self, duration: Duration) -> Self {
        self.handshake_timeout = duration;
        self
    }

    /// Sets the time in which a ping packet is sent to a remote node if no other messages have been sent recently.
    pub fn heartbeat_interval(mut self, duration: Duration) -> Self {
        self.heartbeat_interval = duration;
        self
    }

    /// Sets the time it takes to consider a remote node down when not receiving any data.
    pub fn heartbeat_timeout(mut self, duration: Duration) -> Self {
        self.handshake_timeout = duration;
        self
    }
}

impl Default for NodeOptions {
    fn default() -> Self {
        Self::new()
    }
}
