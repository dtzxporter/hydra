use std::fmt::Debug;
use std::net::SocketAddr;

use crate::start_local_node;
use crate::ExitReason;
use crate::NodeOptions;
use crate::NodeRegistration;
use crate::Process;
use crate::NODE_REGISTRY;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Node {
    Local,
    Remote(String, SocketAddr),
}

impl Node {
    /// Returns `true` if the local node is alive.
    pub fn alive() -> bool {
        NODE_REGISTRY.read().unwrap().contains_key(&0)
    }

    pub fn start<T: Into<String>>(name: T, options: NodeOptions) {
        let name = name.into();

        let mut registry = NODE_REGISTRY.write().unwrap();

        if registry.contains_key(&0) {
            drop(registry);
            panic!("Node is already started!");
        }

        let process = Process::spawn(start_local_node(name.clone(), options));
        let registration = NodeRegistration::new(process, name, options.broadcast_address);

        registry.insert(0, registration);
    }

    pub fn stop() {
        let mut registry = NODE_REGISTRY.write().unwrap();

        if let Some(node_process) = registry.remove(&0) {
            Process::exit(node_process.process, ExitReason::Kill);
        }
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
