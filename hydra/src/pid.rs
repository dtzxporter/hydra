use std::fmt::Debug;

use serde::Deserialize;
use serde::Serialize;

pub const PID_ID_BITS: u64 = 48;
pub const PID_ID_MAXIMUM: u64 = (1 << PID_ID_BITS) - 1;

/// A unique identifier of a process in hydra.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pid {
    /// Packed integer representing the locally indexed node of the process,
    /// and the id of the process on that node.
    pub(crate) packed: u64,
}

impl Pid {
    /// Constructs a new [Pid] from the given process id, assigning it to the local node.
    pub(crate) const fn local(id: u64) -> Self {
        Self::new(0, id)
    }

    /// Constructs a new [Pid] from the given node id and process id.
    pub(crate) const fn new(node: u64, id: u64) -> Self {
        Self {
            packed: ((node & 0xFFFF) << PID_ID_BITS) | id & 0xFFFFFFFFFFFF,
        }
    }

    /// Constructs a new [Pid] from the given packed value.
    pub(crate) const fn _packed(packed: u64) -> Self {
        Self { packed }
    }

    /// Returns true if this [Pid] is a local process.
    pub(crate) const fn is_local(&self) -> bool {
        self.node() == 0
    }

    /// Fetches the node id of the [Pid].
    pub(crate) const fn node(&self) -> u64 {
        (self.packed >> PID_ID_BITS) & 0xFFFF
    }

    /// Fetches the process id of the [Pid].
    pub(crate) const fn id(&self) -> u64 {
        self.packed & 0xFFFFFFFFFFFF
    }
}

impl Debug for Pid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pid<{}, {}>", self.node(), self.id())
    }
}

impl Serialize for Pid {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        unimplemented!()
    }
}

impl<'de> Deserialize<'de> for Pid {
    fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        unimplemented!()
    }
}
