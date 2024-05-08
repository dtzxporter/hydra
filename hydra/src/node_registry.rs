use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::net::SocketAddr;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use dashmap::mapref::entry::Entry;
use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::frame::Frame;

use crate::alias_destroy;
use crate::link_destroy;
use crate::monitor_destroy;
use crate::node_local_supervisor;
use crate::node_remote_connector;
use crate::process_exit_signal_linked;
use crate::process_sender;
use crate::Dest;
use crate::ExitReason;
use crate::Local;
use crate::Node;
use crate::NodeOptions;
use crate::NodeRegistration;
use crate::NodeRemoteSenderMessage;
use crate::NodeState;
use crate::Pid;
use crate::Process;
use crate::ProcessItem;
use crate::Reference;

/// Represents the node id always used for the local node.
pub const LOCAL_NODE_ID: u64 = 0;

/// Represents a node id that will never be allocated.
pub const INVALID_NODE_ID: u64 = u64::MAX;

/// The type of node monitor that was installed.
#[derive(Debug)]
enum NodeMonitor {
    /// The monitor is explicitly for the node itself.
    Node(u64),
    /// The monitor is installed on behalf of a remote process monitor.
    ProcessMonitor(u64, Dest),
    /// The monitor is installed on behalf of a remote process monitor for cleanup.
    ProcessMonitorCleanup(u64),
}

// When a pid is serialized over the wire, we need to lookup it's node@ip:port combination.
// If it's already in the registry, we need to get it's node id, else
// we need to get it's thing.
static NODE_REGISTRATIONS: Lazy<DashMap<u64, NodeRegistration>> = Lazy::new(DashMap::new);

/// A collection of node:id into the node registrations.
static NODE_MAP: Lazy<DashMap<Node, u64>> = Lazy::new(DashMap::new);

/// A collection of node monitors installed.
static NODE_MONITORS: Lazy<DashMap<Node, BTreeMap<Reference, NodeMonitor>>> =
    Lazy::new(DashMap::new);

/// A collection of node links installed.
static NODE_LINKS: Lazy<DashMap<Node, BTreeSet<(Pid, u64)>>> = Lazy::new(DashMap::new);

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
        LOCAL_NODE_ID,
        NodeRegistration::new(
            Some(supervisor),
            NodeState::Current,
            name,
            options.broadcast_address,
        ),
    );

    entry.insert(LOCAL_NODE_ID);

    supervisor
}

/// Stops the local node, forgetting all nodes.
pub fn node_local_stop() {
    let Some((_, _)) = NODE_MAP.remove(&Node::Local) else {
        panic!("Local node not started!");
    };

    NODE_MAP.clear();

    if let Some(entry) = NODE_REGISTRATIONS.get(&LOCAL_NODE_ID) {
        if let Some(supervisor) = entry.supervisor {
            Process::exit(supervisor, ExitReason::Kill);
        }
    }

    NODE_REGISTRATIONS.clear();
    NODE_PENDING_MESSAGES.clear();
}

/// Cleans up distribution information when the local node goes down unexpectedly.
pub fn node_local_panic() {
    NODE_MAP.clear();
    NODE_REGISTRATIONS.clear();
    NODE_PENDING_MESSAGES.clear();
}

/// Returns the process responsible for the local node.
pub fn node_local_process() -> Option<Pid> {
    NODE_REGISTRATIONS
        .get(&LOCAL_NODE_ID)
        .and_then(|process| process.supervisor)
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
                    if let Some(current_supervisor) = value.supervisor.take() {
                        if supervisor != current_supervisor {
                            Process::exit(current_supervisor, ExitReason::Kill);
                        }
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

    let node = Node::from((name.clone(), address));

    let entry = match NODE_MAP.entry(node.clone()) {
        Entry::Vacant(entry) => entry,
        Entry::Occupied(entry) => {
            let id = *entry.get();

            if connect {
                NODE_REGISTRATIONS.alter(&id, |_, mut value| {
                    if value.supervisor.is_none() {
                        value.supervisor = Some(Process::spawn(node_remote_connector(node)));
                        value.state = NodeState::Pending;
                    }

                    value
                });
            }

            return id;
        }
    };

    let next_id = NODE_ID.fetch_add(1, Ordering::Relaxed);

    if connect {
        let supervisor = Process::spawn(node_remote_connector(node));

        NODE_REGISTRATIONS.insert(
            next_id,
            NodeRegistration::new(Some(supervisor), NodeState::Pending, name, address),
        );
    } else {
        NODE_REGISTRATIONS.insert(
            next_id,
            NodeRegistration::new(None, NodeState::Known, name, address),
        );
    }

    entry.insert(next_id);

    next_id
}

/// Triggered when a remote node supervisor goes down unexpectedly.
pub fn node_remote_supervisor_down(node: Node, process: Pid) {
    let Some(id) = NODE_MAP.get(&node) else {
        return;
    };

    NODE_REGISTRATIONS.alter(&id, |_, mut value| {
        if value
            .supervisor
            .is_some_and(|supervisor| supervisor != process)
        {
            return value;
        }

        value.supervisor = None;
        value.sender = None;
        value.receiver = None;
        value.state = NodeState::Known;

        if let Some((_, links)) = NODE_LINKS.remove(&node) {
            for (from, process_id) in links {
                let process = Pid::local(process_id);

                link_destroy(process, from);

                process_exit_signal_linked(process, from, ExitReason::from("noconnection"));
            }
        }

        if let Some((_, monitors)) = NODE_MONITORS.remove(&node) {
            for (reference, monitor) in monitors {
                match monitor {
                    NodeMonitor::Node(id) => {
                        process_sender(Pid::local(id)).map(|sender| {
                            sender.send(ProcessItem::MonitorNodeDown(node.clone(), reference))
                        });
                    }
                    NodeMonitor::ProcessMonitor(id, dest) => {
                        process_sender(Pid::local(id)).map(|sender| {
                            sender.send(ProcessItem::MonitorProcessDown(
                                dest,
                                reference,
                                ExitReason::from("noconnection"),
                            ))
                        });
                    }
                    NodeMonitor::ProcessMonitorCleanup(id) => {
                        monitor_destroy(Pid::local(id), reference);
                    }
                }

                if reference.is_local() {
                    alias_destroy(reference);
                }
            }
        }

        NODE_PENDING_MESSAGES.remove(&node);

        value
    });
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
    let Some(id) = NODE_MAP.get(&node) else {
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
        .get(&LOCAL_NODE_ID)
        .map(|registration| (registration.name.clone(), registration.broadcast_address))
}

/// Looks up the node information for a remote node id.
pub fn node_lookup_remote(id: u64) -> Option<(String, SocketAddr)> {
    NODE_REGISTRATIONS
        .get(&id)
        .map(|registration| (registration.name.clone(), registration.broadcast_address))
}

/// Creates a monitor for the given node and reference from the given process.
pub fn node_monitor_create(node: Node, reference: Reference, from: Pid) {
    NODE_MONITORS
        .entry(node)
        .or_default()
        .insert(reference, NodeMonitor::Node(from.id()));
}

/// Creates a monitor for the given node and reference from the given process for dest.
pub fn node_process_monitor_create(node: Node, reference: Reference, dest: Dest, from: Pid) {
    NODE_MONITORS
        .entry(node)
        .or_default()
        .insert(reference, NodeMonitor::ProcessMonitor(from.id(), dest));
}

/// Creates a monitor cleanup for the given node and reference from the given process.
pub fn node_process_monitor_cleanup(node: Node, reference: Reference, process: Pid) {
    NODE_MONITORS
        .entry(node)
        .or_default()
        .insert(reference, NodeMonitor::ProcessMonitorCleanup(process.id()));
}

/// Destroys a node process monitor for the given node and reference.
pub fn node_process_monitor_destroy(node: Node, reference: Reference) {
    NODE_MONITORS.alter(&node, |_, mut value| {
        value.remove(&reference);
        value
    });
}

/// Destroys all process monitors for the given node by their references.
pub fn node_process_monitor_destroy_all(node: Node, references: Vec<Reference>) {
    NODE_MONITORS.alter(&node, |_, mut value| {
        for reference in references {
            value.remove(&reference);
        }
        value
    });
}

/// Destroys a node process link for the given node by the link process.
pub fn node_process_link_destroy(node: Node, link: Pid, from: Pid) {
    NODE_LINKS.alter(&node, |_, mut value| {
        value.remove(&(link, from.id()));
        value
    });
}

/// Destroys all process links for the given node by their link processes.
pub fn node_process_link_destroy_all(node: Node, links: Vec<Pid>, from: Pid) {
    NODE_LINKS.alter(&node, |_, mut value| {
        for link in links {
            value.remove(&(link, from.id()));
        }
        value
    });
}

/// Creates a monitor for the given node and process from the given linked process.
pub fn node_process_link_create(node: Node, process: Pid, from: Pid) {
    NODE_LINKS
        .entry(node)
        .or_default()
        .insert((process, from.id()));
}

/// Removes a monitor for the given node and reference.
pub fn node_monitor_destroy(node: Node, reference: Reference) {
    NODE_MONITORS.alter(&node, |_, mut value| {
        value.remove(&reference);
        value
    });
}

/// Removes a link for the given node and process.
pub fn node_link_destroy(node: Node, process: Pid, from: Pid) {
    NODE_LINKS.alter(&node, |_, mut value| {
        value.remove(&(process, from.id()));
        value
    });
}

/// Fires when a remote process has notified the local node that it went down for a monitor.
pub fn node_process_monitor_down(node: Node, reference: Reference, exit_reason: ExitReason) {
    let mut monitor: Option<NodeMonitor> = None;

    NODE_MONITORS.alter(&node, |_, mut value| {
        monitor = value.remove(&reference);

        value
    });

    alias_destroy(reference);

    if let Some(NodeMonitor::ProcessMonitor(id, dest)) = monitor {
        process_sender(Pid::local(id)).map(|sender| {
            sender.send(ProcessItem::MonitorProcessDown(
                dest,
                reference,
                exit_reason,
            ))
        });
    }
}

/// Fires when a remote process has notified the local node that it went down for a link.
pub fn node_process_link_down(node: Node, process: Pid, from: Pid, exit_reason: ExitReason) {
    let mut found = false;

    NODE_LINKS.alter(&node, |_, mut value| {
        found = value.remove(&(from, process.id()));
        value
    });

    if found {
        process_exit_signal_linked(process, from, exit_reason);
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
