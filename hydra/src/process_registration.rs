use tokio::task::JoinHandle;

use crate::ExitReason;
use crate::ProcessFlags;
use crate::ProcessSend;

/// Process registration information.
pub struct ProcessRegistration {
    /// A handle to the task that this process lives in.
    pub handle: JoinHandle<()>,
    /// The sender of this process.
    pub sender: ProcessSend,
    /// Registered name of this process or [None] when unregistered.
    pub name: Option<String>,
    /// Process flags.
    pub flags: ProcessFlags,
    /// Process exit reason.
    pub exit_reason: Option<ExitReason>,
}

impl ProcessRegistration {
    /// Constructs a new [ProcessRegistration] from a given task handle, and channel.
    pub const fn new(handle: JoinHandle<()>, sender: ProcessSend) -> Self {
        Self {
            handle,
            sender,
            name: None,
            flags: ProcessFlags::empty(),
            exit_reason: None,
        }
    }
}
