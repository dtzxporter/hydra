use std::net::SocketAddr;

use crate::NodeState;
use crate::Pid;

/// Node registration information.
pub struct NodeRegistration {
    /// The process responsible for this node.
    pub supervisor: Option<Pid>,
    /// The state of this node.
    pub state: NodeState,
    /// The name of this node.
    pub name: String,
    /// The broadcast address of this node.
    pub broadcast_address: SocketAddr,
}

impl NodeRegistration {
    /// Constructs a new [NodeRegistration] from a given process, name, and broadcast address.
    pub fn new(supervisor: Pid, name: String, broadcast_address: SocketAddr) -> Self {
        Self {
            supervisor: Some(supervisor),
            state: NodeState::Known,
            name,
            broadcast_address,
        }
    }
}
