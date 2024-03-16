use std::collections::BTreeSet;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use flume::Sender;

use tokio::task::JoinHandle;

use crate::ExitReason;
use crate::MessageState;
use crate::Pid;
use crate::ProcessFlags;

/// Process registration information.
pub struct ProcessRegistration {
    /// A handle to the task that this process lives in.
    pub handle: JoinHandle<()>,
    /// The mailbox of this process.
    pub channel: Sender<MessageState>,
    /// Registered name of this process or [None] when unregistered.
    pub name: Option<String>,
    /// A collection of linked processes.
    pub links: BTreeSet<Pid>,
    /// Process flags.
    pub flags: AtomicU32,
    /// Process exit reason.
    pub exit_reason: ExitReason,
}

impl ProcessRegistration {
    /// Constructs a new [ProcessRegistration] from a given task handle, and channel.
    pub const fn new(handle: JoinHandle<()>, channel: Sender<MessageState>) -> Self {
        Self {
            handle,
            channel,
            name: None,
            links: BTreeSet::new(),
            flags: AtomicU32::new(ProcessFlags::empty().bits()),
            exit_reason: ExitReason::Normal,
        }
    }

    /// Gets the current process flags.
    #[must_use]
    pub fn flags(&self) -> ProcessFlags {
        ProcessFlags::from_bits_retain(self.flags.load(Ordering::Acquire))
    }

    /// Sets the current process flags.
    pub fn set_flags(&self, flags: ProcessFlags) {
        self.flags.store(flags.bits(), Ordering::Release)
    }
}