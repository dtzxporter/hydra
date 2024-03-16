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

impl<T> From<MessageState> for Message<T>
where
    T: Send + 'static,
{
    fn from(value: MessageState) -> Self {
        match value {
            MessageState::SerializedUser(_serialized) => unimplemented!(),
            MessageState::DeserializedUser(deserialized) => {
                Message::User(*deserialized.downcast::<T>().unwrap())
            }
            MessageState::System(system) => Message::System(system),
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
