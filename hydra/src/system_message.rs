use crate::ExitReason;
use crate::Pid;
use crate::Reference;

/// A message sent from the hydra system.
#[derive(Debug, Clone)]
pub enum SystemMessage {
    /// A process has exited with the given reason.
    Exit(Pid, ExitReason),
    /// A monitored process went down.
    ProcessDown(Pid, Reference, ExitReason),
}
