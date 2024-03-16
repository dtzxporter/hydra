use crate::ExitReason;
use crate::Monitor;
use crate::Pid;

/// A message sent from the hydra system.
pub enum SystemMessage {
    /// A process has exited with the given reason.
    Exit(Pid, ExitReason),
    /// A monitored process went down.
    ProcessDown(Pid, Monitor, ExitReason),
}
