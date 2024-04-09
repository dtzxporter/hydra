use serde::Deserialize;
use serde::Serialize;

/// Represents the reason a process exits.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExitReason {
    /// Process exited due to normal reasons, function ended, or manually stopped.
    Normal,
    /// Process was killed.
    Kill,
    /// Process custom exit reason.
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
