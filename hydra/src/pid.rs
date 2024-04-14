use std::fmt::Debug;
use std::num::NonZeroU64;

use serde::Deserialize;
use serde::Serialize;

/// A unique identifier of a process in hydra.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pid {
    Local(NonZeroU64),
    Remote(NonZeroU64, u64),
}

impl Pid {
    /// Constructs a new [Pid] from the given process id, assigning it to the local node.
    pub(crate) fn local(id: u64) -> Self {
        Self::Local(NonZeroU64::new(id).unwrap())
    }

    /// Returns true if this [Pid] is a local process.
    pub(crate) const fn is_local(&self) -> bool {
        matches!(self, Self::Local(_))
    }

    /// Returns true if this [Pid] is a remote process.
    pub(crate) const fn is_remote(&self) -> bool {
        matches!(self, Self::Remote(_, _))
    }

    /// Gets the id part of the [Pid].
    pub(crate) const fn id(&self) -> u64 {
        match self {
            Self::Local(id) => id.get(),
            Self::Remote(id, _) => id.get(),
        }
    }
}

impl Debug for Pid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id) => write!(f, "Pid<0, {}>", id),
            Self::Remote(id, node) => write!(f, "Pid<{}, {}>", node, id),
        }
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
