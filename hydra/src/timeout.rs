use serde::Deserialize;
use serde::Serialize;

/// Occurs when an operation has timed out.
#[derive(Debug, Serialize, Deserialize)]
pub struct Timeout;
