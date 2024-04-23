use std::fmt::Debug;
use std::net::SocketAddr;

use crate::node_alive;
use crate::node_disconnect;
use crate::node_forget;
use crate::node_list;
use crate::node_list_filtered;
use crate::node_local_start;
use crate::node_local_stop;
use crate::node_register;
use crate::node_set_cookie;
use crate::NodeOptions;
use crate::NodeState;
use crate::Pid;

/// Represents a local or remote node of processes.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Node {
    Local,
    Remote(String, SocketAddr),
}

impl Node {
    /// Returns `true` if the local node is alive.
    pub fn alive() -> bool {
        node_alive()
    }

    /// Returns the current node.
    pub fn current() -> Node {
        Node::Local
    }

    /// Connects to the given node if we're not already connected.
    pub fn connect<T: Into<Node>>(node: T) {
        let node = node.into();

        let Node::Remote(name, address) = node else {
            panic!("Can't connect to self!");
        };

        node_register(Node::from((name, address)), true);
    }

    /// Forcefully disconnects from the given node.
    pub fn disconnect<T: Into<Node>>(node: T) {
        let node = node.into();

        if !matches!(node, Node::Remote(_, _)) {
            panic!("Can't disconnect from self!");
        }

        node_disconnect(node);
    }

    /// Forcefully disconnects, and forgets this node completely.
    ///
    /// All existing [Pid]'s for this node will no longer be reachable.
    pub fn forget<T: Into<Node>>(node: T) {
        let node = node.into();

        if !matches!(node, Node::Remote(_, _)) {
            panic!("Can't forget from self!");
        }

        node_forget(node);
    }

    /// Sets the node cookie used to secure node connections.
    pub fn set_cookie<T: Into<String>>(cookie: T) {
        node_set_cookie(Some(cookie.into()));
    }

    /// Clears the node cookie.
    pub fn clear_cookie() {
        node_set_cookie(None);
    }

    /// Turns a non-distributed node into a distributed node.
    pub fn start<T: Into<String>>(name: T, options: NodeOptions) -> Pid {
        node_local_start(name.into(), options)
    }

    /// Turns a distributed node into a non-distributed node.
    ///
    /// For other nodes in the network, this is the same as the node going down.
    pub fn stop() {
        node_local_stop();
    }

    /// Returns a list of all visible nodes in the system, excluding the local node.
    pub fn list() -> Vec<Node> {
        node_list()
    }

    /// Returns a list of all nodes in the system that match the given state.
    pub fn list_by_state(state: NodeState) -> Vec<Node> {
        node_list_filtered(state)
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local => write!(f, "Node<local>"),
            Self::Remote(name, address) => write!(f, "Node<{}, {}>", name, address),
        }
    }
}

impl From<(&str, SocketAddr)> for Node {
    fn from(value: (&str, SocketAddr)) -> Self {
        Self::Remote(value.0.into(), value.1)
    }
}

impl From<(String, SocketAddr)> for Node {
    fn from(value: (String, SocketAddr)) -> Self {
        Self::Remote(value.0, value.1)
    }
}

impl From<&str> for Node {
    fn from(value: &str) -> Self {
        if value == "local" {
            return Self::Local;
        }

        let split = value.split_once('@').expect("Missing '@' in node string!");

        Self::Remote(
            split.0.into(),
            split.1.parse().expect("Malformed address in node string!"),
        )
    }
}

impl From<String> for Node {
    fn from(value: String) -> Self {
        Node::from(value.as_str())
    }
}

impl PartialEq<&str> for Node {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Self::Local => *other == "local",
            Self::Remote(name, address) => {
                let Some((other_name, other_address)) = other.split_once('@') else {
                    return false;
                };

                if name != other_name {
                    return false;
                }

                let Ok(parse_address) = other_address.parse::<SocketAddr>() else {
                    return false;
                };

                *address == parse_address
            }
        }
    }
}

impl PartialEq<String> for Node {
    fn eq(&self, other: &String) -> bool {
        self == &other.as_str()
    }
}

impl PartialEq<Node> for String {
    fn eq(&self, other: &Node) -> bool {
        other == self
    }
}

impl PartialEq<Node> for &str {
    fn eq(&self, other: &Node) -> bool {
        other == self
    }
}
