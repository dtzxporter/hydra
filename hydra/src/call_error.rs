use serde::Deserialize;
use serde::Serialize;

use crate::ExitReason;
use crate::Timeout;

/// Occurs when a server call fails.
#[derive(Debug, Serialize, Deserialize)]
pub enum CallError {
    Timeout(Timeout),
    ServerDown(ExitReason),
}
