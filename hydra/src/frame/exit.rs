use bincode::Decode;
use bincode::Encode;

use crate::ExitReason;

/// The frame used to send an exit signal to a process.
#[derive(Debug, Encode, Decode)]
pub struct Exit {
    pub process_id: u64,
    pub from_id: u64,
    pub exit_reason: ExitReason,
}

impl Exit {
    /// Constructs a new instance of [Exit] frame.
    pub const fn new(process_id: u64, from_id: u64, exit_reason: ExitReason) -> Self {
        Self {
            process_id,
            from_id,
            exit_reason,
        }
    }
}
