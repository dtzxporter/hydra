use serde::Deserialize;
use serde::Serialize;

use crate::Pid;

/// Debug information for a specific process.
#[derive(Debug, Serialize, Deserialize)]
pub struct ProcessInfo {
    /// The name, if any, that the process was registered under.
    pub registered_name: Option<String>,
    /// The number of messages in this processes message queue.
    pub message_queue_len: usize,
    /// Whether or not the process is trapping exits.
    pub trap_exit: bool,
    /// Collection of linked processes.
    pub links: Vec<Pid>,
    /// Collection of processes monitoring this process.
    pub monitored_by: Vec<Pid>,
}

impl ProcessInfo {
    /// Construct a new empty [ProcessInfo].
    pub const fn new() -> Self {
        Self {
            registered_name: None,
            message_queue_len: 0,
            trap_exit: false,
            links: Vec::new(),
            monitored_by: Vec::new(),
        }
    }
}

impl Default for ProcessInfo {
    fn default() -> Self {
        Self::new()
    }
}
