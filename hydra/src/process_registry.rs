use std::collections::HashMap;
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::ExitReason;
use crate::MessageState;
use crate::Pid;
use crate::ProcessFlags;
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

    /// Forwards an exit signal to the linked [Pid] from the given [Pid] with the `exit_reason`.
    pub fn exit_signal_linked_process(&mut self, pid: Pid, from: Pid, exit_reason: ExitReason) {
        if let Some(process) = self.processes.get_mut(&pid.id()) {
            if process.flags().contains(ProcessFlags::TRAP_EXIT) {
                process
                    .channel
                    .send(MessageState::System(SystemMessage::Exit(
                        from,
                        exit_reason.clone(),
                    )))
                    .unwrap();
            } else if !exit_reason.is_normal() {
                process.exit_reason = exit_reason;
                process.handle.abort();
            }
        }
    }
}

/// Registry of processes currently running on this hydra node.
pub static PROCESS_REGISTRY: Lazy<RwLock<ProcessRegistry>> =
    Lazy::new(|| RwLock::new(ProcessRegistry::new()));
