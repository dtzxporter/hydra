use crate::ExitReason;
use crate::Timeout;

/// Occurs when a server call fails.
#[derive(Debug)]
pub enum CallError {
    Timeout(Timeout),
    ServerDown(ExitReason),
}
