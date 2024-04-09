use std::fmt::Debug;

use serde::Deserialize;
use serde::Serialize;

use crate::Pid;

/// A reference to a process monitor.
#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Monitor {
    pid: Pid,
    reference: u64,
}

impl Monitor {
    /// Constructs a new [Monitor] from the given [Pid] and reference number.
    pub(crate) const fn new(pid: Pid, reference: u64) -> Self {
        Self { pid, reference }
    }

    /// Returns the process id that this monitor is installed for.
    pub(crate) const fn pid(&self) -> Pid {
        self.pid
    }

    /// Returns the reference id for this monitor.
    pub(crate) const fn reference(&self) -> u64 {
        self.reference
    }
}

impl Debug for Monitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Monitor<{:?}, {}>", self.pid, self.reference)
    }
}
