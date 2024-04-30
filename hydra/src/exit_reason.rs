use bincode::Decode;
use bincode::Encode;

use serde::Deserialize;
use serde::Serialize;

/// Represents the reason a process exits.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize, Encode, Decode)]
pub enum ExitReason {
    /// Exited due to normal reasons, function ended, or manually stopped.
    #[default]
    Normal,
    /// Forceful kill reason.
    Kill,
    /// Ignored reason.
    Ignore,
    /// Custom exit reason.
    Custom(String),
}

impl ExitReason {
    /// Whether or not the reason is normal.
    pub const fn is_normal(&self) -> bool {
        matches!(self, ExitReason::Normal)
    }

    /// Whether or not the reason is kill.
    pub const fn is_kill(&self) -> bool {
        matches!(self, ExitReason::Kill)
    }

    /// Whether or not the reason is ignore.
    pub const fn is_ignore(&self) -> bool {
        matches!(self, ExitReason::Ignore)
    }

    /// Whether or not the reason is custom.
    pub const fn is_custom(&self) -> bool {
        matches!(self, ExitReason::Custom(_))
    }
}

impl From<String> for ExitReason {
    fn from(value: String) -> Self {
        Self::Custom(value)
    }
}

impl From<&str> for ExitReason {
    fn from(value: &str) -> Self {
        Self::Custom(value.to_string())
    }
}

impl PartialEq<&str> for ExitReason {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Self::Custom(reason) => reason == other,
            _ => false,
        }
    }
}

impl PartialEq<ExitReason> for &str {
    fn eq(&self, other: &ExitReason) -> bool {
        match other {
            ExitReason::Custom(reason) => self == reason,
            _ => false,
        }
    }
}
