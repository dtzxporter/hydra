use serde::Deserialize;
use serde::Serialize;

/// The different states a node can be in.
#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum NodeState {
    /// This node is the local node.
    Current,
    /// This node information was given to us, but no attempt to connect has been made.
    Known,
    /// This node is currently connected.
    Connected,
    /// This node is pending a connection.
    Pending,
}
