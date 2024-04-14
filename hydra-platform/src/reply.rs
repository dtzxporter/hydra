use std::marker::PhantomData;

use hydra::Pid;
use hydra::Process;
use hydra::Receivable;
use hydra::Reference;

use crate::GenServerMessage;

#[derive(Debug)]
pub struct Reply<T: Send + 'static> {
    to: Pid,
    tag: Reference,
    _messages: PhantomData<T>,
}

impl<T> Reply<T>
where
    T: Receivable,
{
    pub fn new(to: Pid, tag: Reference) -> Self {
        Self {
            to,
            tag,
            _messages: PhantomData,
        }
    }

    pub fn to(&self) -> Pid {
        self.to
    }

    pub fn tag(&self) -> Reference {
        self.tag
    }

    pub fn reply(self, message: T) {
        Process::send(self.to, GenServerMessage::CallReply(self.tag, message));
    }
}
