use serde::Deserialize;
use serde::Serialize;

use std::fmt::Debug;
use std::num::NonZeroU64;

use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

/// A unique id that pertains to a specific node.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reference {
    Local(NonZeroU64),
    Remote(NonZeroU64, u64),
}

impl Reference {
    /// Constructs a new unique [Reference] for this node.
    pub fn new() -> Self {
        static REF: AtomicU64 = AtomicU64::new(1);

        Self::Local(NonZeroU64::new(REF.fetch_add(1, Ordering::Relaxed)).unwrap())
    }

    /// Returns true if this [Reference] is a local process.
    pub(crate) const fn is_local(&self) -> bool {
        matches!(self, Self::Local(_))
    }

    /// Returns true if this [Reference] is a remote process.
    #[allow(unused)]
    pub(crate) const fn is_remote(&self) -> bool {
        matches!(self, Self::Remote(_, _))
    }

    /// Gets the id part of this [Reference].
    pub(crate) const fn id(&self) -> u64 {
        match self {
            Self::Local(id) => id.get(),
            Self::Remote(id, _) => id.get(),
        }
    }
}

impl Default for Reference {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id) => write!(f, "Reference<0, {}>", id),
            Self::Remote(id, node) => write!(f, "Reference<{}, {}", node, id),
        }
    }
}

impl Serialize for Reference {
    fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        unimplemented!()
    }
}

impl<'de> Deserialize<'de> for Reference {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        unimplemented!()
    }
}