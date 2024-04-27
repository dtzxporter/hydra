use std::collections::HashMap;
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::ExitReason;
use crate::Pid;
use crate::ProcessFlags;
use crate::ProcessItem;
use crate::ProcessRegistration;
use crate::SystemMessage;

/// The global process registry.
pub struct ProcessRegistry {
    /// A collection of process id -> process registration.
    pub processes: HashMap<u64, ProcessRegistration>,
    /// A collection of registered named processes.
    pub named_processes: HashMap<String, u64>,
}

impl ProcessRegistry {
    /// Constructs a new [ProcessRegistry].
    pub fn new() -> Self {
        Self {
            processes: HashMap::new(),
            named_processes: HashMap::new(),
        }
    }

    /// Processes an exit signal for the given [Pid] with the `exit_reason`.
    pub fn exit_process(&mut self, pid: Pid, from: Pid, exit_reason: ExitReason) {
        let Some(process) = self.processes.get_mut(&pid.id()) else {
            return;
        };

        let trapping_exits = process.flags().contains(ProcessFlags::TRAP_EXIT);

        match exit_reason {
            ExitReason::Normal => {
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
                if trapping_exits {
                    process
                        .sender
                        .send(ProcessItem::SystemMessage(SystemMessage::Exit(
                            from,
                            exit_reason,
                        )))
                        .unwrap();
                } else {
                    process.exit_reason = Some(exit_reason);
                    process.handle.abort();
                }
            }
        }
    }

    /// Forwards an exit signal to the linked [Pid] from the given [Pid] with the `exit_reason`.
    pub fn exit_signal_linked_process(&mut self, pid: Pid, from: Pid, exit_reason: ExitReason) {
        if let Some(process) = self.processes.get_mut(&pid.id()) {
            if process.flags().contains(ProcessFlags::TRAP_EXIT) {
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
        }
    }
}

/// Registry of processes currently running on this hydra node.
pub static PROCESS_REGISTRY: Lazy<RwLock<ProcessRegistry>> =
    Lazy::new(|| RwLock::new(ProcessRegistry::new()));
