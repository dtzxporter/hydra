use crate::CloseCode;
use crate::WebsocketMessage;

/// A command returned from a websocket callback.
pub enum WebsocketCommand {
    /// A command to send a message.
    Send(WebsocketMessage),
    /// A command to close a websocket.
    Close((CloseCode, String)),
}

impl WebsocketCommand {
    /// Sends the message to the websocket client.
    pub fn send<T: Into<WebsocketMessage>>(message: T) -> Self {
        Self::Send(message.into())
    }

    /// Gracefully close the websocket with the given `code` and `reason`.
    pub fn close<T: Into<String>>(code: CloseCode, reason: T) -> Self {
        Self::Close((code, reason.into()))
    }
}

impl From<WebsocketMessage> for WebsocketCommand {
    fn from(value: WebsocketMessage) -> Self {
        Self::Send(value)
    }
}
