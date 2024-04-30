use serde::Deserialize;
use serde::Serialize;

use crate::Pid;
use crate::Reference;

/// Information about a GenServer call request.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct From {
    pid: Pid,
    tag: Reference,
}

impl From {
    /// Constructs a new instance of [From].
    pub(crate) const fn new(pid: Pid, tag: Reference) -> Self {
        Self { pid, tag }
    }

    /// Gets the process which sent this call.
    pub const fn pid(&self) -> Pid {
        self.pid
    }

    /// Gets the unique identifier for call.
    pub const fn tag(&self) -> Reference {
        self.tag
    }
}
