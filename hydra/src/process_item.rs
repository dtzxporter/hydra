use std::fmt::Debug;

use crate::Dest;
use crate::ExitReason;
use crate::Message;
use crate::Reference;
use crate::SystemMessage;

pub enum ProcessItem {
    /// Sent from a remote process in a different userspace.
    #[allow(unused)]
    UserRemoteMessage(Vec<u8>),
    /// Sent from a process in the same userspace.
    UserLocalMessage(Box<dyn std::any::Any + Send>),
    /// Sent from the system.
    SystemMessage(SystemMessage),
    /// Sent from a monitor when a process goes down.
    MonitorProcessDown(Dest, Reference, ExitReason),
    /// Sent from the system when an alias is deactivated externally.
    AliasDeactivated(u64),
}

impl From<SystemMessage> for ProcessItem {
    fn from(value: SystemMessage) -> Self {
        Self::SystemMessage(value)
    }
}

impl<T> From<Message<T>> for ProcessItem
where
    T: Send + 'static,
{
    fn from(value: Message<T>) -> Self {
        match value {
            Message::User(user) => Self::UserLocalMessage(Box::new(user)),
            Message::System(system) => Self::SystemMessage(system),
        }
    }
}

impl Debug for ProcessItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UserRemoteMessage(_) => write!(f, "UserRemoteMessage(..)"),
            Self::UserLocalMessage(_) => write!(f, "UserLocalMessage(..)"),
            Self::SystemMessage(system) => write!(f, "SystemMessage({:?})", system),
            Self::MonitorProcessDown(dest, reference, exit_reason) => write!(
                f,
                "MonitorProcessDown({:?}, {:?}, {:?})",
                dest, reference, exit_reason
            ),
            Self::AliasDeactivated(alias) => write!(f, "AliasDeactivated({:?})", alias),
        }
    }
}
