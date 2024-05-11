use std::borrow::Cow;

use serde::Deserialize;
use serde::Serialize;

use crate::Node;
use crate::Pid;
use crate::Reference;

/// A process destination.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Dest {
    /// A process id.
    Pid(Pid),
    /// A registered process name.
    Named(Cow<'static, str>, Node),
    /// A reference to an alias.
    Alias(Reference),
}

/// One or more process destinations.
#[derive(Debug)]
pub enum Dests {
    /// A single process destination.
    Dest(Dest),
    /// Mutiple process destinations.
    Dests(Vec<Dest>),
}

impl Dest {
    /// Returns `true` if the [Dest] is for a local process.
    pub const fn is_local(&self) -> bool {
        match self {
            Self::Pid(pid) => pid.is_local(),
            Self::Named(_, node) => node.is_local(),
            Self::Alias(reference) => reference.is_local(),
        }
    }

    /// Returns `true` if the [Dest] is for a remote process.
    pub const fn is_remote(&self) -> bool {
        match self {
            Self::Pid(pid) => pid.is_remote(),
            Self::Named(_, node) => node.is_remote(),
            Self::Alias(reference) => reference.is_remote(),
        }
    }
}

impl From<Pid> for Dest {
    fn from(value: Pid) -> Self {
        Self::Pid(value)
    }
}

impl From<&'static str> for Dest {
    fn from(value: &'static str) -> Self {
        Self::Named(value.into(), Node::Local)
    }
}

impl From<String> for Dest {
    fn from(value: String) -> Self {
        Self::Named(value.into(), Node::Local)
    }
}

impl<T> From<(&'static str, T)> for Dest
where
    T: Into<Node>,
{
    fn from(value: (&'static str, T)) -> Self {
        Self::Named(value.0.into(), value.1.into())
    }
}

impl From<Reference> for Dest {
    fn from(value: Reference) -> Self {
        Self::Alias(value)
    }
}

impl PartialEq<Pid> for Dest {
    fn eq(&self, other: &Pid) -> bool {
        match self {
            Self::Pid(pid) => pid == other,
            _ => false,
        }
    }
}

impl PartialEq<Dest> for Pid {
    fn eq(&self, other: &Dest) -> bool {
        match other {
            Dest::Pid(pid) => self == pid,
            _ => false,
        }
    }
}

impl PartialEq<&str> for Dest {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Self::Named(name, _) => name == other,
            _ => false,
        }
    }
}

impl PartialEq<Dest> for &str {
    fn eq(&self, other: &Dest) -> bool {
        match other {
            Dest::Named(name, _) => self == name,
            _ => false,
        }
    }
}

impl PartialEq<Reference> for Dest {
    fn eq(&self, other: &Reference) -> bool {
        match self {
            Self::Alias(reference) => reference == other,
            _ => false,
        }
    }
}

impl PartialEq<Dest> for Reference {
    fn eq(&self, other: &Dest) -> bool {
        match other {
            Dest::Alias(reference) => reference == other,
            _ => false,
        }
    }
}

impl From<Pid> for Dests {
    fn from(value: Pid) -> Self {
        Self::Dest(Dest::from(value))
    }
}

impl From<Reference> for Dests {
    fn from(value: Reference) -> Self {
        Self::Dest(Dest::from(value))
    }
}

impl From<&'static str> for Dests {
    fn from(value: &'static str) -> Self {
        Self::Dest(Dest::Named(value.into(), Node::Local))
    }
}

impl<T> From<(&'static str, T)> for Dests
where
    T: Into<Node>,
{
    fn from(value: (&'static str, T)) -> Self {
        Self::Dest(Dest::Named(value.0.into(), value.1.into()))
    }
}

impl From<String> for Dests {
    fn from(value: String) -> Self {
        Self::Dest(Dest::Named(value.into(), Node::Local))
    }
}

impl From<&[Pid]> for Dests {
    fn from(value: &[Pid]) -> Self {
        if value.len() == 1 {
            Self::Dest(Dest::from(value[0]))
        } else {
            Self::Dests(value.iter().copied().map(Into::into).collect())
        }
    }
}

impl From<Dest> for Dests {
    fn from(value: Dest) -> Self {
        Self::Dest(value)
    }
}

impl From<&[Dest]> for Dests {
    fn from(value: &[Dest]) -> Self {
        if value.len() == 1 {
            Self::Dest(value[0].to_owned())
        } else {
            Self::Dests(Vec::from(value))
        }
    }
}

impl From<Vec<Dest>> for Dests {
    fn from(value: Vec<Dest>) -> Self {
        Self::Dests(value)
    }
}

impl From<Vec<Pid>> for Dests {
    fn from(value: Vec<Pid>) -> Self {
        Self::Dests(value.into_iter().map(Into::into).collect())
    }
}

impl From<&Vec<Pid>> for Dests {
    fn from(value: &Vec<Pid>) -> Self {
        Self::Dests(value.iter().copied().map(Into::into).collect())
    }
}

impl FromIterator<Dest> for Dests {
    fn from_iter<T: IntoIterator<Item = Dest>>(iter: T) -> Self {
        Self::Dests(Vec::from_iter(iter))
    }
}

impl FromIterator<Pid> for Dests {
    fn from_iter<T: IntoIterator<Item = Pid>>(iter: T) -> Self {
        Self::Dests(Vec::from_iter(iter.into_iter().map(Into::into)))
    }
}
