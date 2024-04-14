use std::collections::BTreeMap;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::ExitReason;
use crate::Pid;
use crate::ProcessItem;
use crate::Reference;
use crate::SystemMessage;
use crate::PROCESS_REGISTRY;

/// A collection of the local processes being monitored, and the references that require the message.
static MONITORED_PROCESSES: Lazy<DashMap<u32, BTreeMap<Reference, Pid>>> = Lazy::new(DashMap::new);

/// Creates a monitor for the given local process and reference from the given process.
pub fn monitor_create(process: Pid, reference: Reference, from: Pid) {
    MONITORED_PROCESSES
        .entry(process.id())
        .or_default()
        .insert(reference, from);
}

/// Destroys a monitor for the given local process and reference, returning `true` if the monitor existed.
pub fn monitor_destroy(process: Pid, reference: Reference) {
    MONITORED_PROCESSES.alter(&process.id(), |_, mut value| {
        value.remove(&reference);
        value
    });
}

/// Destroys all monitors registered for the given reference, pid combination.
pub fn monitor_destroy_all<'a, M: IntoIterator<Item = (&'a Reference, &'a Pid)>>(monitors: M) {
    for (reference, pid) in monitors {
        if pid.is_local() {
            MONITORED_PROCESSES.alter(&pid.id(), |_, mut value| {
                value.remove(reference);
                value
            });
        } else {
            unimplemented!("Remote process monitor not supported!")
        }
    }
}

/// Sends monitor messages about the given process going down for the given reason.
pub fn monitor_process_down(from: Pid, exit_reason: ExitReason) {
    let Some(references) = MONITORED_PROCESSES
        .remove(&from.id())
        .map(|(_, references)| references)
    else {
        return;
    };

    for (reference, pid) in references {
        if pid.is_local() {
            PROCESS_REGISTRY
                .read()
                .unwrap()
                .processes
                .get(&pid.id())
                .map(|process| {
                    process
                        .channel
                        .send(ProcessItem::SystemMessage(SystemMessage::ProcessDown(
                            from,
                            reference,
                            exit_reason.clone(),
                        )))
                });
        } else {
            unimplemented!("Remote process monitor not supported!");
        }
    }
}
