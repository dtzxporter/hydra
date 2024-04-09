use serde::de::DeserializeOwned;

use std::fmt::Debug;

use crate::Message;
use crate::SystemMessage;

/// An internal message state based on where it came from.
pub enum MessageState {
    /// Sent from a remote process in a different userspace.
    SerializedUser(Vec<u8>),
    /// Sent from a process in the same userspace.
    DeserializedUser(Box<dyn std::any::Any + Send>),
    /// Sent from the system.
    System(SystemMessage),
}

impl<T> TryFrom<MessageState> for Message<T>
where
    T: DeserializeOwned + Send + 'static,
{
    type Error = MessageState;

    fn try_from(value: MessageState) -> Result<Self, Self::Error> {
        match value {
            MessageState::SerializedUser(serialized) => {
                #[cfg(feature = "json")]
                {
                    serde_json::from_slice(&serialized)
                        .map(|x| Message::User(x))
                        .map_err(|_| MessageState::SerializedUser(serialized))
                }

                #[cfg(not(any(feature = "json")))]
                unimplemented!()
            }
            MessageState::DeserializedUser(deserialized) => deserialized
                .downcast::<T>()
                .map(|x| Message::User(*x))
                .map_err(MessageState::DeserializedUser),
            MessageState::System(system) => Ok(Message::System(system)),
        }
    }
}

impl From<SystemMessage> for MessageState {
    fn from(value: SystemMessage) -> Self {
        Self::System(value)
    }
}

impl<T> From<Message<T>> for MessageState
where
    T: Send + 'static,
{
    fn from(value: Message<T>) -> Self {
        match value {
            Message::User(user) => Self::DeserializedUser(Box::new(user)),
            Message::System(system) => Self::System(system),
        }
    }
}

impl Debug for MessageState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MessageState::SerializedUser(_) => write!(f, "SerializedUser(..)"),
            MessageState::DeserializedUser(_) => write!(f, "DeserializedUser(..)"),
            MessageState::System(system) => write!(f, "System({:?})", system),
        }
    }
}
