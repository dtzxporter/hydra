use std::marker::PhantomData;

use hydra::Monitor;
use hydra::Pid;
use hydra::Process;

use crate::GenServerMessage;

#[derive(Debug)]
pub struct Reply<T: Send + 'static> {
    to: Pid,
    tag: Monitor,
    _messages: PhantomData<T>,
}

impl<T> Reply<T>
where
    T: Send + 'static,
{
    pub fn new(to: Pid, tag: Monitor) -> Self {
        Self {
            to,
            tag,
            _messages: PhantomData,
        }
    }

    pub fn to(&self) -> Pid {
        self.to
    }

    pub fn tag(&self) -> Monitor {
        self.tag
    }

    pub fn reply(self, message: T) {
        Process::send(self.to, GenServerMessage::CallReply(self.tag, message));
    }
}
