use std::net::SocketAddr;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use dashmap::mapref::entry::Entry;
use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::frame::Frame;

use crate::node_local_supervisor;
use crate::ExitReason;
use crate::Local;
use crate::Node;
use crate::NodeOptions;
use crate::NodeRegistration;
use crate::NodeRemoteSenderMessage;
use crate::NodeState;
use crate::Pid;
use crate::Process;

/// Represents a node id that will never be allocated.
pub const INVALID_NODE_ID: u64 = u64::MAX;

// When a pid is serialized over the wire, we need to lookup it's node@ip:port combination.
// If it's already in the registry, we need to get it's node id, else
// we need to get it's thing.
static NODE_REGISTRATIONS: Lazy<DashMap<u64, NodeRegistration>> = Lazy::new(DashMap::new);

/// A collection of node:id into the node registrations.
static NODE_MAP: Lazy<DashMap<Node, u64>> = Lazy::new(DashMap::new);

/// A collection of node:vec<msg> pending messages for a node.
static NODE_PENDING_MESSAGES: Lazy<DashMap<Node, Vec<Frame>>> = Lazy::new(DashMap::new);

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

    let supervisor = Process::spawn(node_local_supervisor(name.clone(), options));

    NODE_REGISTRATIONS.insert(
        0,
        NodeRegistration::new(
            Some(supervisor),
            NodeState::Current,
            name,
            options.broadcast_address,
        ),
    );

    entry.insert(0);

    supervisor
}

/// Stops the local node, forgetting all nodes.
pub fn node_local_stop() {
    let Some((_, _)) = NODE_MAP.remove(&Node::Local) else {
        panic!("Local node not started!");
    };

    NODE_MAP.clear();

    if let Some(entry) = NODE_REGISTRATIONS.get(&0) {
        if let Some(supervisor) = entry.supervisor {
            Process::exit(supervisor, ExitReason::Kill);
        }
    }

    NODE_REGISTRATIONS.clear();
}

/// Sets the send and receive processes for a given node and flushes any pending messages.
pub fn node_set_send_recv(node: Node, sender: Pid, receiver: Pid) {
    let Some(entry) = NODE_MAP.get(&node) else {
        return;
    };

    NODE_REGISTRATIONS.alter(&entry, |_, mut value| {
        value.sender = Some(sender);
        value.receiver = Some(receiver);

        let frames = NODE_PENDING_MESSAGES
            .remove(&node)
            .map(|pending| pending.1)
            .unwrap_or_default();

        // We need to pop the pending messages and send them to the sender
        // This way, the order for sent messages is maintained and all future messages go direct to the sender.
        Process::send(
            sender,
            NodeRemoteSenderMessage::SendFrames(Local::new(frames)),
        );

        value
    });
}

/// Gets the send process for a given node.
pub fn node_send_frame(frame: Frame, id: u64) {
    let Some(registration) = NODE_REGISTRATIONS.get(&id) else {
        return;
    };

    if let Some(sender) = registration.sender {
        Process::send(
            sender,
            NodeRemoteSenderMessage::SendFrame(Local::new(frame)),
        );
    } else if !matches!(registration.state, NodeState::Known) {
        NODE_PENDING_MESSAGES
            .entry(Node::from((
                registration.name.clone(),
                registration.broadcast_address,
            )))
            .or_default()
            .push(frame);
    }
}

/// Accepts a remote node's connection if one doesn't exist, returns `true` if accepted.
pub fn node_accept(node: Node, supervisor: Pid) -> bool {
    let Node::Remote(name, address) = node else {
        panic!("Can't accept a local node!");
    };

    let entry = NODE_MAP.entry(Node::from((name.clone(), address)));

    match entry {
        Entry::Vacant(entry) => {
            let next_id = NODE_ID.fetch_add(1, Ordering::Relaxed);

            NODE_REGISTRATIONS.insert(
                next_id,
                NodeRegistration::new(Some(supervisor), NodeState::Connected, name, address),
            );

            entry.insert(next_id);

            true
        }
        Entry::Occupied(entry) => {
            let mut accepted = false;

            NODE_REGISTRATIONS.alter(entry.get(), |_, mut value| {
                if matches!(value.state, NodeState::Pending) {
                    if let Some(supervisor) = value.supervisor.take() {
                        Process::exit(supervisor, ExitReason::Kill);
                    }
                }

                if value.supervisor.is_none() {
                    accepted = true;

                    value.supervisor = Some(supervisor);
                    value.state = NodeState::Connected;
                }

                value
            });

            accepted
        }
    }
}

/// Registers a remote node's information, or returns an existing one.
pub fn node_register(node: Node, connect: bool) -> u64 {
    let Node::Remote(name, address) = node else {
        panic!("Can't register a local node!");
    };

    let entry = match NODE_MAP.entry(Node::from((name.clone(), address))) {
        Entry::Vacant(entry) => entry,
        Entry::Occupied(entry) => {
            let node = *entry.get();

            if connect {
                NODE_REGISTRATIONS.alter(&node, |_, mut value| {
                    if value.supervisor.is_none() {
                        value.supervisor = None;
                        value.state = NodeState::Pending;

                        panic!("actually connect lol");
                    }

                    value
                });
            }

            return node;
        }
    };

    let next_id = NODE_ID.fetch_add(1, Ordering::Relaxed);

    if connect {
        NODE_REGISTRATIONS.insert(
            next_id,
            NodeRegistration::new(None, NodeState::Pending, name, address),
        );

        panic!("actually connect lol");
    } else {
        NODE_REGISTRATIONS.insert(
            next_id,
            NodeRegistration::new(None, NodeState::Known, name, address),
        );
    }

    entry.insert(next_id);

    next_id
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
                Some(Node::from((entry.name.clone(), entry.broadcast_address)))
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
        NODE_PENDING_MESSAGES.remove(&node);

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

    NODE_PENDING_MESSAGES.remove(&node);

    if let Some(supervisor) = registration.supervisor {
        Process::exit(supervisor, ExitReason::Kill);
    }
}

/// Looks up the node information for the local node.
pub fn node_lookup_local() -> Option<(String, SocketAddr)> {
    NODE_REGISTRATIONS
        .get(&0)
        .map(|registration| (registration.name.clone(), registration.broadcast_address))
}

/// Looks up the node information for a remote node id.
pub fn node_lookup_remote(id: u64) -> Option<(String, SocketAddr)> {
    NODE_REGISTRATIONS
        .get(&id)
        .map(|registration| (registration.name.clone(), registration.broadcast_address))
}

/// Gets the cookie secret value.
pub fn node_get_cookie() -> Option<String> {
    NODE_COOKIE.lock().unwrap().clone()
}

/// Sets or clears the cookie secret value.
pub fn node_set_cookie(cookie: Option<String>) {
    *NODE_COOKIE.lock().unwrap() = cookie;
}
