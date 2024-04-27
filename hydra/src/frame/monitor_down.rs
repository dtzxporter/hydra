use bincode::Decode;
use bincode::Encode;

use crate::ExitReason;

/// The frame used to notify processes about down monitors.
#[derive(Debug, Encode, Decode)]
pub struct MonitorDown {
    pub monitors: Vec<u64>,
    pub exit_reason: ExitReason,
}

impl MonitorDown {
    /// Constructs a new instance of [MonitorDown] frame.
    pub const fn new(exit_reason: ExitReason) -> Self {
        Self {
            monitors: Vec::new(),
            exit_reason,
        }
    }
}
