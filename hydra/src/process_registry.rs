use std::time::Duration;
use std::time::Instant;

use hydra_dashmap::mapref::entry::Entry;
use hydra_dashmap::DashMap;

use once_cell::sync::Lazy;

use tokio::task::JoinHandle;

use crate::ArgumentError;
use crate::ExitReason;
use crate::Pid;
use crate::ProcessFlags;
use crate::ProcessItem;
use crate::ProcessRegistration;
use crate::ProcessSend;
use crate::Reference;
use crate::SystemMessage;

/// A collection of process id -> process registration.
static PROCESS_REGISTRY: Lazy<DashMap<u64, ProcessRegistration>> = Lazy::new(DashMap::new);
/// A collection of registered named processes.
static PROCESS_NAMES: Lazy<DashMap<String, u64>> = Lazy::new(DashMap::new);
/// A collection of process timers.
static PROCESS_TIMERS: Lazy<DashMap<u64, (Instant, JoinHandle<()>)>> = Lazy::new(DashMap::new);

/// Checks for the given `pid` and calls the given `callback` with the result if it exists or not.
///
/// The process will exist for the duration of `callback`.
pub fn process_exists_lock<C: FnOnce(bool) -> R, R>(pid: Pid, callback: C) -> R {
    if PROCESS_REGISTRY.get(&pid.id()).is_some() {
        callback(true)
    } else {
        callback(false)
    }
}

/// Drops a process from the registry.
pub fn process_drop(pid: Pid) -> Option<ProcessRegistration> {
    PROCESS_REGISTRY
        .remove(&pid.id())
        .map(|(_, process)| process)
}

/// Gets the sender for this process.
pub fn process_sender(pid: Pid) -> Option<ProcessSend> {
    PROCESS_REGISTRY
        .get(&pid.id())
        .map(|process| process.sender.clone())
}

/// Looks up a process by the given name.
pub fn process_name_lookup(name: &str) -> Option<Pid> {
    PROCESS_NAMES
        .get(name)
        .map(|process_id| Pid::local(*process_id))
}

/// Inserts a new process registration.
pub fn process_insert(id: u64, registration: ProcessRegistration) {
    PROCESS_REGISTRY.insert(id, registration);
}

/// Removes a registered name.
pub fn process_name_remove(name: &str) {
    PROCESS_NAMES.remove(name);
}

/// Checks if the process is alive.
pub fn process_alive(pid: Pid) -> bool {
    if pid.is_remote() {
        panic!("Expected a local pid!");
    }

    PROCESS_REGISTRY
        .get(&pid.id())
        .map(|process| process.exit_reason.is_none())
        .unwrap_or_default()
}

/// Registers a process under the given name.
pub fn process_register(pid: Pid, name: String) -> Result<(), ArgumentError> {
    if pid.is_remote() {
        return Err(ArgumentError::from("Expected local pid for register!"));
    }

    let entry = PROCESS_NAMES.entry(name.clone());

    let entry = match entry {
        Entry::Occupied(entry) => {
            if *entry.get() == pid.id() {
                return Ok(());
            } else {
                return Err(ArgumentError::from(format!(
                    "Name {:?} registered to another process!",
                    name
                )));
            }
        }
        Entry::Vacant(entry) => entry,
    };

    let mut updated = false;
    let mut found = false;

    PROCESS_REGISTRY.alter(&pid.id(), |_, mut process| {
        found = true;

        if process.name.is_none() {
            process.name = Some(name);
            updated = true;
        }

        process
    });

    if !found {
        return Err(ArgumentError::from("Process does not exist!"));
    }

    if !updated {
        return Err(ArgumentError::from(format!(
            "Process {:?} was already registered!",
            pid
        )));
    }

    entry.insert(pid.id());

    Ok(())
}

/// Unregisters a process with the given name.
pub fn process_unregister(name: &str) {
    let Some((_, pid)) = PROCESS_NAMES.remove(name) else {
        panic!("Name {:?} was not registered!", name);
    };

    PROCESS_REGISTRY.alter(&pid, |_, mut process| {
        process.name = None;
        process
    });
}

/// Processes an exit signal for the given [Pid] with the `exit_reason`.
pub fn process_exit(pid: Pid, from: Pid, exit_reason: ExitReason) {
    PROCESS_REGISTRY.alter(&pid.id(), |_, mut process| {
        let trapping_exits = process.flags.contains(ProcessFlags::TRAP_EXIT);

        match exit_reason {
            ExitReason::Normal | ExitReason::Ignore => {
                if pid == from {
                    process.exit_reason = Some(exit_reason);
                    process.handle.abort();
                } else if trapping_exits {
                    process
                        .sender
                        .send(ProcessItem::SystemMessage(SystemMessage::Exit(
                            from,
                            exit_reason,
                        )))
                        .unwrap();
                }
            }
            ExitReason::Kill => {
                process.exit_reason = Some(exit_reason);
                process.handle.abort();
            }
            ExitReason::Custom(_) => {
                if pid == from || !trapping_exits {
                    process.exit_reason = Some(exit_reason);
                    process.handle.abort();
                } else {
                    process
                        .sender
                        .send(ProcessItem::SystemMessage(SystemMessage::Exit(
                            from,
                            exit_reason,
                        )))
                        .unwrap();
                }
            }
        }

        process
    });
}

/// Forwards an exit signal to the linked [Pid] from the given [Pid] with the `exit_reason`.
pub fn process_exit_signal_linked(pid: Pid, from: Pid, exit_reason: ExitReason) {
    PROCESS_REGISTRY.alter(&pid.id(), |_, mut process| {
        if process.flags.contains(ProcessFlags::TRAP_EXIT) {
            process
                .sender
                .send(ProcessItem::SystemMessage(SystemMessage::Exit(
                    from,
                    exit_reason.clone(),
                )))
                .unwrap();
        } else if !exit_reason.is_normal() {
            process.exit_reason = Some(exit_reason);
            process.handle.abort();
        }

        process
    });
}

/// Returns the process flags for the given process.
pub fn process_flags(pid: Pid) -> Option<ProcessFlags> {
    PROCESS_REGISTRY.get(&pid.id()).map(|process| process.flags)
}

/// Sets the process flags.
pub fn process_set_flags(pid: Pid, flags: ProcessFlags) {
    PROCESS_REGISTRY.alter(&pid.id(), |_, mut process| {
        process.flags = flags;
        process
    });
}

/// Sets the process exit reason.
pub fn process_set_exit_reason(pid: Pid, exit_reason: ExitReason) {
    PROCESS_REGISTRY.alter(&pid.id(), |_, mut process| {
        process.exit_reason = Some(exit_reason);
        process
    });
}

/// Returns a list of processes.
pub fn process_list() -> Vec<Pid> {
    PROCESS_REGISTRY
        .iter()
        .map(|process| Pid::local(*process.key()))
        .collect()
}

/// Returns a list of registered process names.
pub fn process_name_list() -> Vec<String> {
    PROCESS_NAMES
        .iter()
        .map(|value| value.key().to_owned())
        .collect()
}

/// Registers a timer.
pub fn process_register_timer(timer: Reference, duration: Duration, handle: JoinHandle<()>) {
    PROCESS_TIMERS.insert(timer.id(), (Instant::now() + duration, handle));
}

/// Reads a timer.
pub fn process_read_timer(timer: Reference) -> Option<Duration> {
    let time = PROCESS_TIMERS.get(&timer.id())?;

    let now = Instant::now();
    let timer = time.value().0;

    if timer <= now {
        Some(Duration::from_secs(0))
    } else {
        Some(timer - now)
    }
}

/// Unregisters and kills a timer.
pub fn process_destroy_timer(timer: Reference) {
    if let Some((_, (_, handle))) = PROCESS_TIMERS.remove(&timer.id()) {
        handle.abort();
    }
}
