use crate::Message;
use crate::ProcessItem;
use crate::Receivable;
use crate::SystemMessage;
use crate::PROCESS;

/// Used to receive messages from processes.
pub struct ProcessReceiver {
    ignore_type: bool,
}

impl ProcessReceiver {
    /// Constructs a new instance of [ProcessReceiver].
    pub(crate) fn new() -> Self {
        Self { ignore_type: false }
    }

    /// Ignore messages that don't match the given type.
    ///
    /// By default, `receive`/`match` will panic if the type doesn't match.
    pub fn ignore_type(mut self) -> Self {
        self.ignore_type = true;
        self
    }

    /// Selects a single message.
    ///
    /// This will panic if `ignore_type` was not called and the type doesn't match.
    pub async fn select<T: Receivable, F: (Fn(&Message<&T>) -> bool) + Send>(
        self,
        filter: F,
    ) -> Message<T> {
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

    /// Receives a single message.
    ///
    /// This will panic if `ignore_type` was not called and the type doesn't match.
    pub async fn receive<T: Receivable>(self) -> Message<T> {
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
        ProcessItem::AliasDeactivated(_) => unreachable!(),
    }
}

/// Processes a process item, which could be a command, or a message, or anything.
#[inline(always)]
fn process_item<T: Receivable>(item: &mut ProcessItem) -> Result<Option<Message<&T>>, ()> {
    // Special case for serialized messages, we'll convert them to deserialized one time to prevent
    // deserializing the value more than once, then convert to a reference.
    if let ProcessItem::UserRemoteMessage(serialized) = item {
        #[cfg(feature = "json")]
        {
            let result: Result<T, _> = serde_json::from_slice(serialized);

            if let Ok(result) = result {
                *item = ProcessItem::UserLocalMessage(Box::new(result));
            } else {
                return Err(());
            }
        }

        #[cfg(not(any(feature = "json")))]
        unimplemented!()
    }

    match item {
        // Explicitly handled first, so that we can process the other values later.
        ProcessItem::UserRemoteMessage(_) => unreachable!(),
        ProcessItem::UserLocalMessage(deserialized) => deserialized
            .downcast_ref()
            .map(Message::User)
            .ok_or(())
            .map(Some),
        ProcessItem::SystemMessage(system) => {
            // Special logic for certain system messages.
            match system {
                SystemMessage::ProcessDown(_, reference, _) => {
                    if PROCESS
                        .with(|process| process.monitors.borrow_mut().remove(reference).is_none())
                    {
                        // If the process has already called demonitor, discard the message.
                        // This prevents the need for a flush option.
                        return Ok(None);
                    }
                }
                _ => {
                    // No special handling.
                }
            }

            Ok(Some(Message::System(system.clone())))
        }
        ProcessItem::AliasDeactivated(id) => {
            PROCESS.with(|process| process.aliases.borrow_mut().remove(id));
            Ok(None)
        }
    }
}
