use crate::SystemMessage;

/// A messages sent to a process.
#[derive(Debug)]
pub enum Message<T> {
    /// A message that was sent from another process.
    User(T),
    /// A message that was sent from the system.
    System(SystemMessage),
}
