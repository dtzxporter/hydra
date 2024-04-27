use bincode::Decode;
use bincode::Encode;

use crate::ExitReason;

/// The frame used to notify processes about down links.
#[derive(Debug, Encode, Decode)]
pub struct LinkDown {
    pub links: Vec<u64>,
    pub from_id: u64,
    pub exit_reason: ExitReason,
}

impl LinkDown {
    /// Constructs a new isntance of [LinkDown] frame.
    pub const fn new(from_id: u64, exit_reason: ExitReason) -> Self {
        Self {
            links: Vec::new(),
            from_id,
            exit_reason,
        }
    }
}
