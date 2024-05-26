use smallvec::SmallVec;

use crate::CloseCode;
use crate::WebsocketMessage;

/// Internal websocket command representation.
pub(crate) enum WebsocketCommand {
    Send(WebsocketMessage),
    Close(CloseCode, String),
}

/// A command buffer returned from a websocket callback.
pub struct WebsocketCommands {
    pub(crate) buffer: SmallVec<[WebsocketCommand; 6]>,
}

impl WebsocketCommands {
    /// Constructs a new websocket command buffer.
    pub fn new() -> Self {
        Self {
            buffer: SmallVec::new(),
        }
    }

    /// Constructs a new websocket command buffer to send `message`.
    pub fn with_send<T: Into<WebsocketMessage>>(message: T) -> Self {
        let mut result = Self::new();

        result.send(message);
        result
    }

    /// Constructs a new websocket command buffer to close the websocket gracefully.
    pub fn with_close<T: Into<String>>(code: CloseCode, reason: T) -> Self {
        let mut result = Self::new();

        result.close(code, reason);
        result
    }

    /// Sends the message to the websocket client.
    pub fn send<T: Into<WebsocketMessage>>(&mut self, message: T) {
        self.buffer.push(WebsocketCommand::Send(message.into()));
    }

    /// Gracefully close the websocket with the given `code` and `reason`.
    pub fn close<T: Into<String>>(&mut self, code: CloseCode, reason: T) {
        self.buffer
            .push(WebsocketCommand::Close(code, reason.into()));
    }
}

impl Default for WebsocketCommands {
    fn default() -> Self {
        Self::new()
    }
}
