use std::collections::BTreeMap;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::frame::Monitor;
use crate::frame::MonitorDown;

use crate::node_lookup_remote;
use crate::node_monitor_destroy;
use crate::node_process_monitor_create;
use crate::node_process_monitor_destroy;
use crate::node_process_monitor_destroy_all;
use crate::node_register;
use crate::node_send_frame;
use crate::Dest;
use crate::ExitReason;
use crate::Node;
use crate::Pid;
use crate::ProcessItem;
use crate::ProcessMonitor;
use crate::Reference;
use crate::PROCESS;
use crate::PROCESS_REGISTRY;

/// A collection of the local processes being monitored, and the references that require the message.
#[allow(clippy::type_complexity)]
static MONITORS: Lazy<DashMap<u64, BTreeMap<Reference, (Pid, Option<Dest>)>>> =
    Lazy::new(DashMap::new);

/// Creates a monitor for the given local process and reference from the given process.
pub fn monitor_create(process: Pid, reference: Reference, from: Pid, dest: Option<Dest>) {
    MONITORS
        .entry(process.id())
        .or_default()
        .insert(reference, (from, dest));
}

/// Destroys a monitor for the given local process and reference.
pub fn monitor_destroy(process: Pid, reference: Reference) {
    if process.is_local() {
        MONITORS.alter(&process.id(), |_, mut value| {
            value.remove(&reference);
            value
        });
    } else {
        let monitor = Monitor::new(false, Some(process.id()), None, None, reference.id());

        node_send_frame(monitor.into(), reference.node());

        if let Some((name, address)) = node_lookup_remote(reference.node()) {
            node_process_monitor_destroy(Node::from((name, address)), reference);
        }
    }
}

/// Destroys all monitors registered for the given reference, monitor combination.
pub fn monitor_destroy_all<'a, M: IntoIterator<Item = (&'a Reference, &'a ProcessMonitor)>>(
    monitors: M,
) {
    for (reference, monitor) in monitors {
        match monitor {
            ProcessMonitor::ForProcess(pid) => {
                let Some(pid) = pid else {
                    continue;
                };

                if pid.is_local() {
                    MONITORS.alter(&pid.id(), |_, mut value| {
                        value.remove(reference);
                        value
                    });
                } else {
                    let monitor = Monitor::new(false, Some(pid.id()), None, None, reference.id());

                    node_send_frame(monitor.into(), reference.node());
                }
            }
            ProcessMonitor::ForNode(node) => {
                node_monitor_destroy(node.clone(), *reference);
            }
        }
    }
}

/// Installs a monitor for the given process.
pub fn monitor_install(process: Dest, reference: Reference, from: Pid) {
    let dest = process.clone();

    let send_process_down = |dest: Dest, exit_reason: ExitReason| {
        PROCESS.with(|process| {
            process
                .sender
                .send(ProcessItem::MonitorProcessDown(
                    dest,
                    reference,
                    exit_reason,
                ))
                .unwrap()
        });
    };

    match process {
        Dest::Pid(pid) => {
            if pid == from {
                panic!("Can not monitor yourself!");
            }

            PROCESS.with(|process| {
                process
                    .monitors
                    .borrow_mut()
                    .insert(reference, ProcessMonitor::ForProcess(Some(pid)))
            });

            if pid.is_local() {
                let registry = PROCESS_REGISTRY.read().unwrap();

                if registry.processes.contains_key(&pid.id()) {
                    monitor_create(pid, reference, from, Some(process));
                } else {
                    send_process_down(dest, ExitReason::from("noproc"));
                }
            } else {
                match node_lookup_remote(pid.node()) {
                    Some((name, address)) => {
                        let node = Node::from((name, address));

                        node_process_monitor_create(node.clone(), reference, dest, from);

                        let node = node_register(node, true);
                        let monitor = Monitor::new(
                            true,
                            Some(pid.id()),
                            None,
                            Some(from.id()),
                            reference.id(),
                        );

                        node_send_frame(monitor.into(), node);
                    }
                    None => {
                        send_process_down(dest, ExitReason::from("noconnection"));
                    }
                }
            }
        }
        Dest::Named(name, node) => {
            if node.is_local() {
                let registry = PROCESS_REGISTRY.read().unwrap();

                let Some(process) = registry.named_processes.get(name.as_ref()) else {
                    PROCESS.with(|process| {
                        process
                            .monitors
                            .borrow_mut()
                            .insert(reference, ProcessMonitor::ForProcess(None))
                    });
                    return send_process_down(dest, ExitReason::from("noproc"));
                };

                let pid = Pid::local(*process);

                PROCESS.with(|process| {
                    process
                        .monitors
                        .borrow_mut()
                        .insert(reference, ProcessMonitor::ForProcess(Some(pid)))
                });

                if registry.processes.contains_key(process) {
                    monitor_create(pid, reference, from, Some(dest));
                } else {
                    send_process_down(dest, ExitReason::from("noproc"));
                }
            } else {
                PROCESS.with(|process| {
                    process
                        .monitors
                        .borrow_mut()
                        .insert(reference, ProcessMonitor::ForProcess(None))
                });

                node_process_monitor_create(node.clone(), reference, dest, from);

                let node = node_register(node.clone(), true);
                let monitor = Monitor::new(
                    true,
                    None,
                    Some(name.into_owned()),
                    Some(from.id()),
                    reference.id(),
                );

                node_send_frame(monitor.into(), node);
            }
        }
        Dest::Alias(_) => panic!("Can not monitor an alias!"),
    }
}

/// Sends monitor messages about the given process going down for the given reason.
pub fn monitor_process_down(from: Pid, exit_reason: ExitReason) {
    let Some(references) = MONITORS
        .remove(&from.id())
        .map(|(_, references)| references)
    else {
        return;
    };

    let mut remote_monitors: BTreeMap<u64, (MonitorDown, Vec<Reference>)> = BTreeMap::new();

    for (reference, (pid, dest)) in references {
        if pid.is_local() {
            PROCESS_REGISTRY
                .read()
                .unwrap()
                .processes
                .get(&pid.id())
                .map(|process| {
                    process.sender.send(ProcessItem::MonitorProcessDown(
                        dest.unwrap(),
                        reference,
                        exit_reason.clone(),
                    ))
                });
        } else {
            let remote = remote_monitors
                .entry(pid.node())
                .or_insert((MonitorDown::new(exit_reason.clone()), Vec::new()));

            remote.0.monitors.push(reference.id());
            remote.1.push(reference);
        }
    }

    for (node, (monitor_down, references)) in remote_monitors {
        if let Some((name, address)) = node_lookup_remote(node) {
            node_process_monitor_destroy_all(Node::from((name, address)), references);
        }

        node_send_frame(monitor_down.into(), node);
    }
}
