use std::borrow::Cow;
use std::num::NonZeroU64;

use bincode::Decode;
use bincode::Encode;

use crate::Pid;
use crate::Reference;

/// The target process information for a send frame.
#[derive(Debug, Encode, Decode)]
pub enum SendTarget {
    Pid(NonZeroU64),
    Named(String),
    Alias(NonZeroU64),
}

/// The frame used to send a message to remote processes.
#[derive(Debug, Encode, Decode)]
pub struct Send {
    pub targets: Vec<SendTarget>,
    pub message: Vec<u8>,
}

impl Send {
    /// Constructs a new [Send] frame with the given message.
    pub fn with_message(message: Vec<u8>) -> Self {
        Self {
            targets: Vec::new(),
            message,
        }
    }

    /// Constructs a new [Send] frame with the given process id.
    pub fn with_pid(id: NonZeroU64, message: Vec<u8>) -> Self {
        Self {
            targets: vec![SendTarget::Pid(id)],
            message,
        }
    }

    /// Constructs a new [Send] frame with the given process name.
    pub fn with_name(name: String, message: Vec<u8>) -> Self {
        Self {
            targets: vec![SendTarget::Named(name)],
            message,
        }
    }

    /// Constructs a new [Send] frame with the given alias.
    pub fn with_alias(alias: NonZeroU64, message: Vec<u8>) -> Self {
        Self {
            targets: vec![SendTarget::Alias(alias)],
            message,
        }
    }
}

impl From<Pid> for SendTarget {
    fn from(value: Pid) -> Self {
        SendTarget::Pid(NonZeroU64::new(value.id()).unwrap())
    }
}

impl From<String> for SendTarget {
    fn from(value: String) -> Self {
        Self::Named(value)
    }
}

impl<'a> From<Cow<'a, str>> for SendTarget {
    fn from(value: Cow<'a, str>) -> Self {
        Self::Named(String::from(value))
    }
}

impl From<Reference> for SendTarget {
    fn from(value: Reference) -> Self {
        Self::Alias(NonZeroU64::new(value.id()).unwrap())
    }
}
