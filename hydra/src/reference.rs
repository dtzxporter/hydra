use std::fmt::Debug;
use std::net::SocketAddr;
use std::num::NonZeroU64;

use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use serde::Deserialize;
use serde::Serialize;

use crate::node_lookup_local;
use crate::node_lookup_remote;
use crate::node_register;
use crate::Node;
use crate::INVALID_NODE_ID;
use crate::LOCAL_NODE_ID;
use crate::SERIALIZE_NODE;

/// The representation of a [Reference] serialized for the wire.
#[derive(Serialize, Deserialize)]
enum ReferenceWire {
    Local(NonZeroU64),
    Remote(NonZeroU64, String, SocketAddr),
    RemoteUnavailable(NonZeroU64),
}

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

    /// Constructs a new [Reference] from the given id, assigning it to the local node.
    pub(crate) fn local(id: u64) -> Self {
        Self::Local(NonZeroU64::new(id).unwrap())
    }

    /// Constructs a new [Reference] from the given id, assigning it to the remote node.
    pub(crate) fn remote(id: u64, node: u64) -> Self {
        Self::Remote(NonZeroU64::new(id).unwrap(), node)
    }

    /// Returns true if this [Reference] is a local process.
    pub(crate) const fn is_local(&self) -> bool {
        matches!(self, Self::Local(_))
    }

    /// Gets the id part of this [Reference].
    pub(crate) const fn id(&self) -> u64 {
        match self {
            Self::Local(id) => id.get(),
            Self::Remote(id, _) => id.get(),
        }
    }

    /// Gets the node part of this [Reference].
    pub(crate) const fn node(&self) -> u64 {
        match self {
            Self::Local(_) => LOCAL_NODE_ID,
            Self::Remote(_, node) => *node,
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
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SERIALIZE_NODE.with(|target| {
            let reference: ReferenceWire = match self {
                Self::Local(id) => {
                    if matches!(target, Node::Local) {
                        ReferenceWire::Local(*id)
                    } else {
                        match node_lookup_local() {
                            Some((name, address)) => {
                                let node = Node::from((name.as_str(), address));

                                if node == *target {
                                    ReferenceWire::Local(*id)
                                } else {
                                    ReferenceWire::Remote(*id, name, address)
                                }
                            }
                            None => ReferenceWire::RemoteUnavailable(*id),
                        }
                    }
                }
                Self::Remote(id, node) => match node_lookup_remote(*node) {
                    Some((name, address)) => {
                        let node = Node::from((name.as_str(), address));

                        if node == *target {
                            ReferenceWire::Local(*id)
                        } else {
                            ReferenceWire::Remote(*id, name, address)
                        }
                    }
                    None => ReferenceWire::RemoteUnavailable(*id),
                },
            };

            reference.serialize(serializer)
        })
    }
}

impl<'de> Deserialize<'de> for Reference {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let node: ReferenceWire = ReferenceWire::deserialize(deserializer)?;

        match node {
            ReferenceWire::Local(id) => Ok(Self::Local(id)),
            ReferenceWire::Remote(id, name, address) => {
                let node = node_register(Node::from((name, address)), false);

                Ok(Self::Remote(id, node))
            }
            ReferenceWire::RemoteUnavailable(id) => Ok(Self::Remote(id, INVALID_NODE_ID)),
        }
    }
}
