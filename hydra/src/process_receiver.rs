use std::marker::PhantomData;

use crate::deserialize_value;
use crate::Message;
use crate::ProcessItem;
use crate::ProcessMonitor;
use crate::Receivable;
use crate::SystemMessage;
use crate::PROCESS;

/// Used to receive messages from processes.
pub struct ProcessReceiver<T: Receivable> {
    ignore_type: bool,
    _message: PhantomData<T>,
}

impl<T> ProcessReceiver<T>
where
    T: Receivable,
{
    /// Constructs a new instance of [ProcessReceiver].
    pub(crate) fn new() -> ProcessReceiver<T> {
        Self {
            ignore_type: true,
            _message: PhantomData,
        }
    }

    /// Enables strict type checking which will panic if the message type doesn't match.
    pub fn strict_type_checking(mut self) -> Self {
        self.ignore_type = false;
        self
    }

    /// Sets the message type this receiver will handle.
    pub fn for_message<N: Receivable>(self) -> ProcessReceiver<N> {
        ProcessReceiver::<N> {
            ignore_type: self.ignore_type,
            _message: PhantomData,
        }
    }

    /// Selects a single message.
    pub async fn select<F: (Fn(&Message<&T>) -> bool) + Send>(self, filter: F) -> Message<T> {
        let result = PROCESS.with(|process| {
            let mut items = process.items.borrow_mut();
            let mut found: Option<usize> = None;

            for (index, item) in items.iter_mut().enumerate() {
                match process_item::<T>(item) {
                    Ok(Some(message)) => {
                        if filter(&message) {
                            found = Some(index);
                            break;
                        }
                    }
                    Ok(None) => {
                        continue;
                    }
                    Err(_) => {
                        if self.ignore_type {
                            continue;
                        } else {
                            panic!("Unsupported message type!")
                        }
                    }
                }
            }

            if let Some(found) = found {
                return Some(convert_item::<T>(items.remove(found)));
            }

            None
        });

        if let Some(message) = result {
            return message;
        }

        let receiver = PROCESS.with(|process| process.receiver.clone());

        loop {
            let mut item = receiver.recv_async().await.unwrap();

            match process_item::<T>(&mut item) {
                Ok(Some(message)) => {
                    if filter(&message) {
                        return convert_item::<T>(item);
                    }

                    PROCESS.with(|process| process.items.borrow_mut().push(item));
                }
                Ok(None) => {
                    continue;
                }
                Err(_) => {
                    if self.ignore_type {
                        PROCESS.with(|process| process.items.borrow_mut().push(item));
                        continue;
                    } else {
                        panic!("Unsupported message type!")
                    }
                }
            }
        }
    }

    /// Removes any message that is already in the message queue matching the filter.
    pub fn remove<F: (FnMut(&Message<&T>) -> bool) + Send>(self, mut filter: F) {
        PROCESS.with(|process| {
            let mut items = process.items.borrow_mut();

            items.retain_mut(|item| match process_item::<T>(item) {
                Ok(Some(message)) => !filter(&message),
                Ok(None) => true,
                Err(_) => {
                    if self.ignore_type {
                        true
                    } else {
                        panic!("Unsupported message type!")
                    }
                }
            });
        });

        let receiver = PROCESS.with(|process| process.receiver.clone());

        for mut item in receiver.drain() {
            match process_item::<T>(&mut item) {
                Ok(Some(message)) => {
                    if !filter(&message) {
                        PROCESS.with(|process| process.items.borrow_mut().push(item));
                    }
                }
                Ok(None) => continue,
                Err(_) => {
                    if self.ignore_type {
                        PROCESS.with(|process| process.items.borrow_mut().push(item));
                        continue;
                    } else {
                        panic!("Unsupported message type!")
                    }
                }
            }
        }
    }

    /// Receives a single message.
    pub async fn receive(self) -> Message<T> {
        self.select(|_| true).await
    }
}

/// Converts a processed item into a message.
#[inline(always)]
fn convert_item<T: Receivable>(item: ProcessItem) -> Message<T> {
    match item {
        // If we got here, the deserialization has already taken place.
        ProcessItem::UserRemoteMessage(_) => unreachable!(),
        ProcessItem::UserLocalMessage(deserialized) => {
            deserialized.downcast().map(|x| Message::User(*x)).unwrap()
        }
        ProcessItem::SystemMessage(system) => Message::System(system),
        // Handled during item processing.
        ProcessItem::MonitorProcessDown(_, _, _) => unreachable!(),
        ProcessItem::MonitorNodeDown(_, _) => unreachable!(),
        ProcessItem::MonitorProcessUpdate(_, _) => unreachable!(),
        ProcessItem::AliasDeactivated(_) => unreachable!(),
    }
}

/// Processes a process item, which could be a command, or a message, or anything.
#[inline(always)]
fn process_item<T: Receivable>(item: &mut ProcessItem) -> Result<Option<Message<&T>>, ()> {
    // Special case for serialized messages, we'll convert them to deserialized one time to prevent
    // deserializing the value more than once, then convert to a reference.
    if let ProcessItem::UserRemoteMessage(serialized) = item {
        let result: Result<T, _> = deserialize_value(serialized);

        if let Ok(result) = result {
            *item = ProcessItem::UserLocalMessage(Box::new(result));
        } else {
            return Err(());
        }
    }

    match item {
        // Explicitly handled first, so that we can process the other values later.
        ProcessItem::UserRemoteMessage(_) => unreachable!(),
        ProcessItem::UserLocalMessage(deserialized) => deserialized
            .downcast_ref()
            .map(Message::User)
            .ok_or(())
            .map(Some),
        ProcessItem::SystemMessage(system) => Ok(Some(Message::System(system.clone()))),
        ProcessItem::MonitorProcessDown(dest, reference, exit_reason) => {
            if PROCESS.with(|process| process.monitors.borrow_mut().remove(reference).is_none()) {
                // If the process has already called demonitor, discard the message.
                // This prevents the need for a flush option.
                return Ok(None);
            }

            let system = SystemMessage::ProcessDown(dest.clone(), *reference, exit_reason.clone());

            // Make sure processing only happens one time.
            *item = ProcessItem::SystemMessage(system.clone());

            Ok(Some(Message::System(system)))
        }
        ProcessItem::MonitorNodeDown(node, reference) => {
            if PROCESS.with(|process| process.monitors.borrow_mut().remove(reference).is_none()) {
                // If the process has already called demonitor, discard the message.
                // This prevents the need for a flush option.
                return Ok(None);
            }

            let system = SystemMessage::NodeDown(node.clone(), *reference);

            // Make sure processing only happens one time.
            *item = ProcessItem::SystemMessage(system.clone());

            Ok(Some(Message::System(system)))
        }
        ProcessItem::MonitorProcessUpdate(reference, pid) => {
            // Update the existing monitor reference so that if we go down before it fires,
            // We can gracefully clean up the remote monitor, this is only for named monitors.
            PROCESS.with(|process| {
                if let Some(monitor) = process.monitors.borrow_mut().get_mut(reference) {
                    *monitor = ProcessMonitor::ForProcess(Some(*pid));
                }
            });

            Ok(None)
        }
        ProcessItem::AliasDeactivated(id) => {
            PROCESS.with(|process| process.aliases.borrow_mut().remove(id));
            Ok(None)
        }
    }
}
