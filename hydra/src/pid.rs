use std::fmt::Debug;
use std::net::SocketAddr;
use std::num::NonZeroU64;

use serde::Deserialize;
use serde::Serialize;

use crate::node_lookup_local;
use crate::node_lookup_remote;
use crate::node_register;
use crate::Node;
use crate::INVALID_NODE_ID;
use crate::LOCAL_NODE_ID;
use crate::SERIALIZE_NODE;

/// The representation of a [Pid] serialized for the wire.
#[derive(Serialize, Deserialize)]
enum PidWire {
    Local(NonZeroU64),
    Remote(NonZeroU64, String, SocketAddr),
    RemoteUnavailable(NonZeroU64),
}

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

    /// Gets the node part of the [Pid].
    pub(crate) const fn node(&self) -> u64 {
        match self {
            Self::Local(_) => LOCAL_NODE_ID,
            Self::Remote(_, node) => *node,
        }
    }

    /// Returns true if this [Pid] is a local process.
    pub const fn is_local(&self) -> bool {
        matches!(self, Self::Local(_))
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
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SERIALIZE_NODE.with(|target| {
            let node: PidWire = match self {
                Self::Local(id) => {
                    if matches!(target, Node::Local) {
                        PidWire::Local(*id)
                    } else {
                        match node_lookup_local() {
                            Some((name, address)) => {
                                let node = Node::from((name.as_str(), address));

                                if node == *target {
                                    PidWire::Local(*id)
                                } else {
                                    PidWire::Remote(*id, name, address)
                                }
                            }
                            None => PidWire::RemoteUnavailable(*id),
                        }
                    }
                }
                Self::Remote(id, node) => match node_lookup_remote(*node) {
                    Some((name, address)) => {
                        let node = Node::from((name.as_str(), address));

                        if node == *target {
                            PidWire::Local(*id)
                        } else {
                            PidWire::Remote(*id, name, address)
                        }
                    }
                    None => PidWire::RemoteUnavailable(*id),
                },
            };

            node.serialize(serializer)
        })
    }
}

impl<'de> Deserialize<'de> for Pid {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let node: PidWire = PidWire::deserialize(deserializer)?;

        match node {
            PidWire::Local(id) => Ok(Self::Local(id)),
            PidWire::Remote(id, name, address) => {
                let node = node_register(Node::from((name, address)), false);

                Ok(Self::Remote(id, node))
            }
            PidWire::RemoteUnavailable(id) => Ok(Self::Remote(id, INVALID_NODE_ID)),
        }
    }
}
