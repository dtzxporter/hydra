use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use tokio::task::JoinHandle;

use crate::ExitReason;
use crate::Pid;
use crate::ProcessFlags;
use crate::ProcessSend;

/// Process registration information.
pub struct ProcessRegistration {
    /// A handle to the task that this process lives in.
    pub handle: JoinHandle<()>,
    /// The mailbox of this process.
    pub channel: ProcessSend,
    /// Registered name of this process or [None] when unregistered.
    pub name: Option<String>,
    /// A collection of linked processes.
    pub links: BTreeSet<Pid>,
    /// A collection of monitors for our process.
    pub monitors: BTreeSet<(Pid, u64)>,
    /// A collection of installed monitors.
    pub installed_monitors: BTreeMap<u64, Pid>,
    /// The next monitor id.
    pub next_monitor_id: u64,
    /// Process flags.
    pub flags: AtomicU32,
    /// Process exit reason.
    pub exit_reason: ExitReason,
}

impl ProcessRegistration {
    /// Constructs a new [ProcessRegistration] from a given task handle, and channel.
    pub const fn new(handle: JoinHandle<()>, channel: ProcessSend) -> Self {
        Self {
            handle,
            channel,
            name: None,
            links: BTreeSet::new(),
            monitors: BTreeSet::new(),
            installed_monitors: BTreeMap::new(),
            next_monitor_id: 0,
            flags: AtomicU32::new(ProcessFlags::empty().bits()),
            exit_reason: ExitReason::Normal,
        }
    }

    /// Returns the next monitor id for this process.
    pub fn next_monitor(&mut self) -> u64 {
        let mut next = self.next_monitor_id.wrapping_add(1);

        loop {
            if self.installed_monitors.contains_key(&next) {
                next = self.next_monitor_id.wrapping_add(1);
                continue;
            }

            break;
        }

        self.next_monitor_id = next;

        next
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
