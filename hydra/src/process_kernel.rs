use std::collections::BTreeMap;

use crate::frame::Frame;
use crate::frame::Send;
use crate::frame::SendTarget;

use crate::alias_retrieve;
use crate::node_process_send_with_alias;
use crate::node_process_send_with_name;
use crate::node_process_send_with_pid;
use crate::node_register;
use crate::node_send_frame;
use crate::process_name_lookup;
use crate::process_sender;
use crate::serialize_value;
use crate::Dest;
use crate::Dests;
use crate::Message;
use crate::ProcessItem;
use crate::Receivable;

/// Sends a message to one or more destinations.
pub fn process_send<M: Receivable>(dests: Dests, message: M) {
    match dests {
        Dests::Dest(dest) => {
            process_send_optimal(dest, message);
        }
        Dests::Dests(dests) => {
            let mut first_local_process: Option<Dest> = None;
            let mut serialized: Option<Vec<u8>> = None;

            // Delay serialization until it's absolutely necessary to have it.
            let mut get_serialized = || {
                if let Some(serialized) = &serialized {
                    serialized.clone()
                } else {
                    let message = serialize_value(&message);

                    serialized = Some(message.clone());

                    message
                }
            };

            let mut remote_sends: BTreeMap<u64, Send> = BTreeMap::new();

            for dest in dests {
                if dest.is_local() && first_local_process.is_none() {
                    first_local_process = Some(dest);
                    continue;
                }

                if dest.is_local() {
                    process_send_unoptimal(dest, get_serialized());
                } else {
                    let (node, send_target) = match dest {
                        Dest::Pid(pid) => (pid.node(), SendTarget::from(pid)),
                        Dest::Named(name, node) => {
                            (node_register(node, false), SendTarget::from(name))
                        }
                        Dest::Alias(reference) => (reference.node(), SendTarget::from(reference)),
                    };

                    remote_sends
                        .entry(node)
                        .or_insert(Send::with_message(get_serialized()))
                        .targets
                        .push(send_target);
                }
            }

            // Fast path optimization to move the message itself into the mailbox of one local process destination.
            if let Some(dest) = first_local_process.take() {
                process_send_optimal(dest, message);
            }

            // Optimally send one packet per target node.
            for (node, send) in remote_sends {
                node_send_frame(Frame::from(send), node);
            }
        }
    }
}

/// Sends a single message to the target destination, avoiding T: Clone.
///
/// TODO: This is a suboptimal path, that can be replaced once specialization lands by specializing on T: Clone.
fn process_send_unoptimal(dest: Dest, message: Vec<u8>) {
    match dest {
        Dest::Pid(pid) => {
            process_sender(pid).map(|sender| sender.send(ProcessItem::UserRemoteMessage(message)));
        }
        Dest::Named(name, _) => {
            process_name_lookup(name.as_ref())
                .and_then(process_sender)
                .map(|sender| sender.send(ProcessItem::UserRemoteMessage(message)));
        }
        Dest::Alias(reference) => {
            alias_retrieve(reference)
                .map(|alias| alias.sender.send(ProcessItem::UserRemoteMessage(message)));
        }
    }
}

// Optimally sends a single message to the target destination.
fn process_send_optimal<M: Receivable>(dest: Dest, message: M) {
    match dest {
        Dest::Pid(pid) => {
            if pid.is_local() {
                process_sender(pid).map(|sender| sender.send(Message::User(message).into()));
            } else {
                node_process_send_with_pid(pid, message);
            }
        }
        Dest::Named(name, node) => {
            if node.is_local() {
                process_name_lookup(name.as_ref())
                    .and_then(process_sender)
                    .map(|sender| sender.send(Message::User(message).into()));
            } else {
                node_process_send_with_name(name.into_owned(), node, message);
            }
        }
        Dest::Alias(reference) => {
            if reference.is_local() {
                alias_retrieve(reference)
                    .map(|alias| alias.sender.send(Message::User(message).into()));
            } else {
                node_process_send_with_alias(reference, message);
            }
        }
    }
}
