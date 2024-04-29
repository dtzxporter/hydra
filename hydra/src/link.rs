use std::collections::BTreeMap;
use std::collections::BTreeSet;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::frame::Link;
use crate::frame::LinkDown;

use crate::node_lookup_remote;
use crate::node_process_link_create;
use crate::node_process_link_destroy;
use crate::node_process_link_destroy_all;
use crate::node_register;
use crate::node_send_frame;
use crate::ExitReason;
use crate::Node;
use crate::Pid;
use crate::PROCESS_REGISTRY;

/// A collection of local processes linked to another process.
static LINKS: Lazy<DashMap<u64, BTreeSet<Pid>>> = Lazy::new(DashMap::new);

/// Creates a link for the given local process from the given process. Returns `true` if the link was created.
pub fn link_create(process: Pid, from: Pid, ignore_errors: bool) -> bool {
    if ignore_errors {
        LINKS.entry(process.id()).or_default().insert(from);
        return true;
    }

    if PROCESS_REGISTRY
        .read()
        .unwrap()
        .processes
        .get(&process.id())
        .is_some()
    {
        LINKS.entry(process.id()).or_default().insert(from);
        true
    } else {
        false
    }
}

/// Installs a link for the given process.
pub fn link_install(process: Pid, from: Pid) {
    link_create(from, process, false)
        .then_some(())
        .expect("From process must exist at this point!");

    if process.is_local() {
        if !link_create(process, from, false) {
            PROCESS_REGISTRY
                .write()
                .unwrap()
                .exit_signal_linked_process(from, process, ExitReason::from("noproc"));
        }
    } else {
        match node_lookup_remote(process.node()) {
            Some((name, address)) => {
                let node = Node::from((name, address));

                node_process_link_create(node.clone(), process, from);

                let node = node_register(node, true);
                let link = Link::new(true, process.id(), from.id());

                node_send_frame(link.into(), node);
            }
            None => {
                PROCESS_REGISTRY
                    .write()
                    .unwrap()
                    .exit_signal_linked_process(from, process, ExitReason::from("noconnection"));
            }
        }
    }
}

/// Destroys a link for the given local process.
pub fn link_destroy(process: Pid, from: Pid) {
    if process.is_local() {
        LINKS.alter(&process.id(), |_, mut value| {
            value.remove(&from);
            value
        });
    } else {
        let link = Link::new(false, process.id(), from.id());

        node_send_frame(link.into(), process.node());

        if let Some((name, address)) = node_lookup_remote(process.node()) {
            node_process_link_destroy(Node::from((name, address)), process, from);
        }
    }
}

/// Sends the proper exit signal about the given process going down for the given reason.
pub fn link_process_down(from: Pid, exit_reason: ExitReason) {
    let Some(links) = LINKS.remove(&from.id()).map(|(_, links)| links) else {
        return;
    };

    let mut remote_links: BTreeMap<u64, (LinkDown, Vec<Pid>)> = BTreeMap::new();

    let mut registry = PROCESS_REGISTRY.write().unwrap();

    for pid in links {
        if pid.is_local() {
            LINKS.alter(&pid.id(), |_, mut value| {
                value.remove(&from);
                value
            });

            registry.exit_signal_linked_process(pid, from, exit_reason.clone());
        } else {
            let remote = remote_links
                .entry(pid.node())
                .or_insert((LinkDown::new(from.id(), exit_reason.clone()), Vec::new()));

            remote.0.links.push(pid.id());
            remote.1.push(pid);
        }
    }

    for (node, (link_down, links)) in remote_links {
        if let Some((name, address)) = node_lookup_remote(node) {
            node_process_link_destroy_all(Node::from((name, address)), links, from);
        }

        node_send_frame(link_down.into(), node);
    }
}
