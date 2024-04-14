use std::collections::BTreeMap;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::Dest;
use crate::ExitReason;
use crate::Pid;
use crate::ProcessItem;
use crate::Reference;
use crate::PROCESS;
use crate::PROCESS_REGISTRY;

/// A collection of the local processes being monitored, and the references that require the message.
#[allow(clippy::type_complexity)]
static MONITORS: Lazy<DashMap<u32, BTreeMap<Reference, (Pid, Dest)>>> = Lazy::new(DashMap::new);

/// Creates a monitor for the given local process and reference from the given process.
pub fn monitor_create(process: Pid, reference: Reference, from: Pid, dest: Dest) {
    MONITORS
        .entry(process.id())
        .or_default()
        .insert(reference, (from, dest));
}

/// Destroys a monitor for the given local process and reference, returning `true` if the monitor existed.
pub fn monitor_destroy(process: Pid, reference: Reference) {
    MONITORS.alter(&process.id(), |_, mut value| {
        value.remove(&reference);
        value
    });
}

/// Destroys all monitors registered for the given reference, pid combination.
pub fn monitor_destroy_all<'a, M: IntoIterator<Item = (&'a Reference, &'a Option<Pid>)>>(
    monitors: M,
) {
    for (reference, pid) in monitors {
        let Some(pid) = pid else {
            continue;
        };

        if pid.is_local() {
            MONITORS.alter(&pid.id(), |_, mut value| {
                value.remove(reference);
                value
            });
        } else {
            unimplemented!("Remote process monitor not supported!")
        }
    }
}

/// Installs a monitor for the given process.
pub fn monitor_install(process: Dest, reference: Reference, from: Pid) {
    let dest = process.clone();

    let send_process_down = |dest: Dest| {
        PROCESS.with(|process| {
            process
                .sender
                .send(ProcessItem::MonitorProcessDown(
                    dest,
                    reference,
                    "noproc".into(),
                ))
                .unwrap()
        });
    };

    match process {
        Dest::Pid(pid) => {
            if pid == from {
                panic!("Can not monitor yourself!");
            }

            if pid.is_remote() {
                unimplemented!("Remote process monitor unsupported!");
            }

            PROCESS.with(|process| process.monitors.borrow_mut().insert(reference, Some(pid)));

            let registry = PROCESS_REGISTRY.read().unwrap();

            if registry.processes.contains_key(&pid.id()) {
                monitor_create(pid, reference, from, process);
            } else {
                send_process_down(dest);
            }
        }
        Dest::Named(name) => {
            let registry = PROCESS_REGISTRY.read().unwrap();

            let Some(process) = registry.named_processes.get(name.as_ref()) else {
                PROCESS.with(|process| process.monitors.borrow_mut().insert(reference, None));
                return send_process_down(dest);
            };

            let pid = Pid::local(*process);

            PROCESS.with(|process| process.monitors.borrow_mut().insert(reference, Some(pid)));

            if registry.processes.contains_key(process) {
                monitor_create(pid, reference, from, dest);
            } else {
                send_process_down(dest);
            }
        }
        Dest::Alias(_) => unimplemented!("Alias monitor not supported!"),
        Dest::RemoteNamed(_, _) => unimplemented!("Remote monitor not supported!"),
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

    for (reference, (pid, dest)) in references {
        if pid.is_local() {
            PROCESS_REGISTRY
                .read()
                .unwrap()
                .processes
                .get(&pid.id())
                .map(|process| {
                    process.channel.send(ProcessItem::MonitorProcessDown(
                        dest,
                        reference,
                        exit_reason.clone(),
                    ))
                });
        } else {
            unimplemented!("Remote process monitor not supported!");
        }
    }
}
