use crate::Dest;
use crate::ExitReason;
use crate::Node;
use crate::Pid;
use crate::Reference;

/// A message sent from the hydra system.
#[derive(Debug, Clone)]
pub enum SystemMessage {
    /// A process has exited with the given reason.
    Exit(Pid, ExitReason),
    /// A monitored process went down.
    ProcessDown(Dest, Reference, ExitReason),
    /// A monitored node went down.
    NodeDown(Node, Reference),
}
