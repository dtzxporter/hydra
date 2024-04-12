use std::net::IpAddr;
use std::net::Ipv4Addr;
use std::net::SocketAddr;

#[derive(Clone, Copy)]
pub struct NodeOptions {
    pub(crate) listen_address: SocketAddr,
    pub(crate) broadcast_address: SocketAddr,
}

impl NodeOptions {
    /// Constructs a new instance of [NodeOptions] with default values.
    pub const fn new() -> Self {
        Self {
            listen_address: SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 1337),
            broadcast_address: SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 1337),
        }
    }

    pub fn listen_address<T: Into<SocketAddr>>(mut self, address: T) -> Self {
        self.listen_address = address.into();
        self
    }

    pub fn broadcast_address<T: Into<SocketAddr>>(mut self, address: T) -> Self {
        self.broadcast_address = address.into();
        self
    }
}

impl Default for NodeOptions {
    fn default() -> Self {
        Self::new()
    }
}
