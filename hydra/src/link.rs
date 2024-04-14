use std::collections::BTreeSet;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::ExitReason;
use crate::Pid;
use crate::PROCESS_REGISTRY;

/// A collection of local processes linked to another process.
static LINKS: Lazy<DashMap<u32, BTreeSet<Pid>>> = Lazy::new(DashMap::new);

/// Creates a link for the given local process from the given process.
pub fn link_create(process: Pid, from: Pid) {
    LINKS.entry(process.id()).or_default().insert(from);
}

/// Destroys a link for the given local process.
pub fn link_destroy(process: Pid, from: Pid) {
    LINKS.alter(&process.id(), |_, mut value| {
        value.remove(&from);
        value
    });
}

/// Sends the proper exit signal about the given process going down for the given reason.
pub fn link_process_down(from: Pid, exit_reason: ExitReason) {
    let Some(links) = LINKS.remove(&from.id()).map(|(_, links)| links) else {
        return;
    };

    let mut registry = PROCESS_REGISTRY.write().unwrap();

    for pid in links {
        if pid.is_remote() {
            unimplemented!("Remote process link unsupported!");
        }

        LINKS.alter(&pid.id(), |_, mut value| {
            value.remove(&from);
            value
        });

        registry.exit_signal_linked_process(pid, from, exit_reason.clone());
    }
}
