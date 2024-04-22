use std::num::NonZeroU64;

use bincode::Decode;
use bincode::Encode;

/// The target process information for a send frame.
#[derive(Debug, Encode, Decode)]
pub enum SendTarget {
    Pid(NonZeroU64),
    Named(String),
    Alias(NonZeroU64),
}

/// The frame used to send a message to a remote process.
#[derive(Debug, Encode, Decode)]
pub struct Send {
    pub target: SendTarget,
    pub message: Vec<u8>,
}

impl Send {
    /// Constructs a new [Send] frame with the given process id.
    pub const fn with_pid(id: NonZeroU64, message: Vec<u8>) -> Self {
        Self {
            target: SendTarget::Pid(id),
            message,
        }
    }

    /// Constructs a new [Send] frame with the given process name.
    pub const fn with_name(name: String, message: Vec<u8>) -> Self {
        Self {
            target: SendTarget::Named(name),
            message,
        }
    }

    /// Constructs a new [Send] frame with the given alias.
    pub const fn with_alias(alias: NonZeroU64, message: Vec<u8>) -> Self {
        Self {
            target: SendTarget::Alias(alias),
            message,
        }
    }
}
