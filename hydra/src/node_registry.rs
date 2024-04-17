use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use dashmap::mapref::entry::Entry;
use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::ExitReason;
use crate::Node;
use crate::NodeOptions;
use crate::NodeRegistration;
use crate::NodeState;
use crate::Pid;
use crate::Process;

// When a pid is serialized over the wire, we need to lookup it's node@ip:port combination.
// If it's already in the registry, we need to get it's node id, else
// we need to get it's thing.
static NODE_REGISTRATIONS: Lazy<DashMap<u64, NodeRegistration>> = Lazy::new(DashMap::new);

/// A collection of node:id into the node registrations.
static NODE_MAP: Lazy<DashMap<Node, u64>> = Lazy::new(DashMap::new);

/// A secret value that secures the connection between nodes.
static NODE_COOKIE: Mutex<Option<String>> = Mutex::new(None);

/// The next id for this node.
static NODE_ID: AtomicU64 = AtomicU64::new(1);

/// Returns `true` if the local node is alive.
pub fn node_alive() -> bool {
    NODE_MAP.contains_key(&Node::Local)
}

/// Starts the local node.
pub fn node_local_start(name: String, options: NodeOptions) -> Pid {
    let Entry::Vacant(entry) = NODE_MAP.entry(Node::Local) else {
        panic!("Local node already started!");
    };

    let supervisor = Process::spawn(async move {
        //
        let _ = name;
        let _ = options;
    });

    entry.insert(0);

    supervisor
}

/// Stops the local node, forgetting all nodes.
pub fn node_local_stop() {
    let Some((_, _)) = NODE_MAP.remove(&Node::Local) else {
        panic!("Local node not started!");
    };

    NODE_MAP.clear();

    for entry in NODE_REGISTRATIONS.iter() {
        if let Some(supervisor) = entry.supervisor {
            Process::exit(supervisor, ExitReason::Kill);
        }
    }

    NODE_REGISTRATIONS.clear();
}

/// Registers a connection to a remote node.
pub fn node_register(node: Node, _connect: bool) {
    let Entry::Vacant(entry) = NODE_MAP.entry(node) else {
        return;
    };

    let next_id = NODE_ID.fetch_add(1, Ordering::Relaxed);

    // Spawn and register the node.

    // Insert the entry after setting up the registration so that no one can attempt to map this
    // node before the registration information is populated.
    entry.insert(next_id);
}

/// Returns the node list excluding the local node.
pub fn node_list() -> Vec<Node> {
    NODE_MAP
        .iter()
        .filter_map(|entry| {
            if matches!(entry.key(), Node::Local) {
                None
            } else {
                Some(entry.key().clone())
            }
        })
        .collect()
}

/// Returns the node list filtered to the given node state.
pub fn node_list_filtered(state: NodeState) -> Vec<Node> {
    NODE_REGISTRATIONS
        .iter()
        .filter_map(|entry| {
            if entry.state == state {
                Some((entry.name.clone(), entry.broadcast_address).into())
            } else {
                None
            }
        })
        .collect()
}

/// Disconnects a connected node, leaving it as a known node.
pub fn node_disconnect(node: Node) {
    let Some(id) = NODE_MAP.get(&node).map(|id| *id) else {
        return;
    };

    NODE_REGISTRATIONS.alter(&id, |_, mut value| {
        if let Some(supervisor) = value.supervisor.take() {
            Process::exit(supervisor, ExitReason::Kill);
        }

        value.state = NodeState::Known;
        value
    });
}

/// Disconnects and forgets a node completely.
pub fn node_forget(node: Node) {
    let Some((_, id)) = NODE_MAP.remove(&node) else {
        return;
    };

    let Some((_, registration)) = NODE_REGISTRATIONS.remove(&id) else {
        return;
    };

    if let Some(supervisor) = registration.supervisor {
        Process::exit(supervisor, ExitReason::Kill);
    }
}

/// Gets the cookie secret value.
pub fn node_get_cookie() -> Option<String> {
    NODE_COOKIE.lock().unwrap().clone()
}

/// Sets or clears the cookie secret value.
pub fn node_set_cookie(cookie: Option<String>) {
    *NODE_COOKIE.lock().unwrap() = cookie;
}
