use std::net::SocketAddr;

use crate::Pid;

/// Node registration information.
pub struct NodeRegistration {
    /// The process responsible for this node.
    pub process: Pid,
    /// The name of this node.
    pub name: String,
    /// The broadcast address of this node.
    pub broadcast_address: SocketAddr,
}

impl NodeRegistration {
    /// Constructs a new [NodeRegistration] from a given process, name, and broadcast address.
    pub const fn new(process: Pid, name: String, broadcast_address: SocketAddr) -> Self {
        Self {
            process,
            name,
            broadcast_address,
        }
    }
}
