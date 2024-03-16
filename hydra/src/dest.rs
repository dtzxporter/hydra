use std::borrow::Cow;

use crate::Pid;

/// A process destination.
#[derive(Debug, Clone)]
pub enum Dest {
    /// A process id.
    Pid(Pid),
    /// A registered local process name.
    Named(Cow<'static, str>),
    /// A remote registered process name.
    RemoteNamed(Cow<'static, str>, String),
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

impl From<(&'static str, String)> for Dest {
    fn from(value: (&'static str, String)) -> Self {
        Self::RemoteNamed(value.0.into(), value.1)
    }
}

impl From<(&'static str, &str)> for Dest {
    fn from(value: (&'static str, &str)) -> Self {
        Self::RemoteNamed(value.0.into(), value.1.to_string())
    }
}
