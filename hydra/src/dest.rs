use std::borrow::Cow;

use crate::Node;
use crate::Pid;
use crate::Reference;

/// A process destination.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Dest {
    /// A process id.
    Pid(Pid),
    /// A registered local process name.
    Named(Cow<'static, str>),
    /// A remote registered process name.
    RemoteNamed(Cow<'static, str>, Node),
    /// A reference to an alias.
    Alias(Reference),
}

impl From<Pid> for Dest {
    fn from(value: Pid) -> Self {
        Self::Pid(value)
    }
}

impl From<&'static str> for Dest {
    fn from(value: &'static str) -> Self {
        Self::Named(value.into())
    }
}

impl From<String> for Dest {
    fn from(value: String) -> Self {
        Self::Named(value.into())
    }
}

impl<T> From<(&'static str, T)> for Dest
where
    T: Into<Node>,
{
    fn from(value: (&'static str, T)) -> Self {
        Self::RemoteNamed(value.0.into(), value.1.into())
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
            Self::Named(name) => name == other,
            _ => false,
        }
    }
}

impl PartialEq<Dest> for &str {
    fn eq(&self, other: &Dest) -> bool {
        match other {
            Dest::Named(name) => self == name,
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
