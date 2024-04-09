use serde::de::DeserializeOwned;

use flume::Receiver;
use flume::Sender;

use crate::Message;
use crate::MessageState;
use crate::Pid;
use crate::PROCESS_REGISTRY;

/// A peakable view of a process's mailbox.
pub struct ProcessReceiver {
    /// The process id of the receivers inbox.
    pid: Pid,
    /// The return inbox for kept messages.
    current_send: Option<Sender<MessageState>>,
    /// The inbox temporarily used for sending messages.
    peak_send: Sender<MessageState>,
    /// The inbox temporarily used for receiving messages.
    peak_receiver: Option<Receiver<MessageState>>,
}

impl ProcessReceiver {
    /// Constructs a new receiver channel that allows peaking from the stream, when dropped, it will reset to the original values.
    pub(crate) fn new(
        pid: Pid,
        sender: Sender<MessageState>,
        receiver: Receiver<MessageState>,
    ) -> Self {
        let (tx, rx) = flume::unbounded();

        for message in receiver.drain() {
            tx.send(message).unwrap();
        }

        Self {
            pid,
            current_send: Some(sender),
            peak_send: tx,
            peak_receiver: Some(rx),
        }
    }

    /// Returns a copy of the current receiver's sender.
    pub(crate) fn sender(&self) -> Sender<MessageState> {
        self.peak_send.clone()
    }

    /// Keeps this message in the process's mailbox.
    pub fn keep<M: Into<MessageState>>(&self, message: M) {
        self.current_send
            .as_ref()
            .unwrap()
            .send(message.into())
            .unwrap();
    }

    /// Receives a single message that matches the given type from the current processes mailbox or panics.
    #[must_use]
    pub async fn receive<T: DeserializeOwned + Send + 'static>(&self) -> Message<T> {
        self.peak_receiver
            .as_ref()
            .unwrap()
            .recv_async()
            .await
            .unwrap()
            .try_into()
            .unwrap()
    }

    /// Receives a single filtered message that matches the given type from the current processes mailbox.
    #[must_use]
    pub async fn filter_receive<T: DeserializeOwned + Send + 'static>(&self) -> Message<T> {
        loop {
            let message = self
                .peak_receiver
                .as_ref()
                .unwrap()
                .recv_async()
                .await
                .unwrap();

            match message.try_into() {
                Ok(message) => return message,
                Err(message) => self.current_send.as_ref().unwrap().send(message).unwrap(),
            }
        }
    }
}

impl Drop for ProcessReceiver {
    fn drop(&mut self) {
        let mut registry = PROCESS_REGISTRY.write().unwrap();
        let sender = self.current_send.take().unwrap();

        for message in self.peak_receiver.take().unwrap().drain() {
            sender.send(message).unwrap();
        }

        registry.processes.get_mut(&self.pid.id()).unwrap().channel = sender;
    }
}
