use std::fmt::Debug;

use crate::Pid;

/// A reference to a process monitor.
pub struct Monitor {
    pid: Pid,
    reference: u64,
}

impl Monitor {
    /// Constructs a new [Monitor] from the given [Pid] and reference number.
    pub(crate) const fn new(pid: Pid, reference: u64) -> Self {
        Self { pid, reference }
    }
}

impl Debug for Monitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Monitor<{:?}, {}>", self.pid, self.reference)
    }
}
