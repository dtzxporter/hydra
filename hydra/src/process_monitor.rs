use crate::Node;
use crate::Pid;

/// Represents an installed monitor's data for a process.
pub enum ProcessMonitor {
    /// A monitor installed to monitor the given process.
    ForProcess(Option<Pid>),
    /// A monitor installed to monitor the given node.
    ForNode(Node),
}
