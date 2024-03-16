use flume::Receiver;
use flume::Sender;

use crate::Message;
use crate::MessageState;
use crate::Pid;
use crate::PROCESS_REGISTRY;

/// A peakable view of a process's mailbox.
pub struct ProcessReceiver {
    pid: Pid,
    current_send: Option<Sender<MessageState>>,
    peak_send: Sender<MessageState>,
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

    /// Receives a single message from the current processes mailbox.
    pub async fn receive<T: Send + 'static>(&self) -> Message<T> {
        self.peak_receiver
            .as_ref()
            .unwrap()
            .recv_async()
            .await
            .unwrap()
            .into()
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
