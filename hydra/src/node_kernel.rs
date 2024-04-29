use crate::frame::Exit;
use crate::frame::Frame;
use crate::frame::Send;
use crate::frame::SendTarget;

use crate::alias_retrieve;
use crate::node_register;
use crate::node_send_frame;
use crate::serialize_value;
use crate::ExitReason;
use crate::Node;
use crate::Pid;
use crate::ProcessItem;
use crate::Receivable;
use crate::Reference;
use crate::PROCESS_REGISTRY;

/// Forwards an incoming nodes send frame message to the target if it exists.
pub fn node_forward_send(send: Send) {
    match send.target {
        SendTarget::Pid(id) => {
            PROCESS_REGISTRY
                .read()
                .unwrap()
                .processes
                .get(&id.get())
                .map(|process| {
                    process
                        .sender
                        .send(ProcessItem::UserRemoteMessage(send.message))
                });
        }
        SendTarget::Named(name) => {
            let registry = PROCESS_REGISTRY.read().unwrap();

            registry
                .named_processes
                .get(&name)
                .and_then(|id| registry.processes.get(id))
                .map(|process| {
                    process
                        .sender
                        .send(ProcessItem::UserRemoteMessage(send.message))
                });
        }
        SendTarget::Alias(alias) => {
            alias_retrieve(Reference::Local(alias)).map(|alias| {
                alias
                    .sender
                    .send(ProcessItem::UserRemoteMessage(send.message))
            });
        }
    }
}

/// Sends an exit signal to the given pid.
pub fn node_process_send_exit(pid: Pid, from: Pid, exit_reason: ExitReason) {
    let exit = Exit::new(pid.id(), from.id(), exit_reason);

    node_send_frame(exit.into(), pid.node());
}

/// Sends the given message to the remote node with the given pid.
pub fn node_process_send_with_pid<M: Receivable>(pid: Pid, message: M) {
    let Pid::Remote(id, node) = pid else {
        panic!("Can't send to a local process!");
    };

    let message = serialize_value(&message);

    node_send_frame(Frame::from(Send::with_pid(id, message)), node);
}

/// Sends the given message to the remote node with the given alias.
pub fn node_process_send_with_alias<M: Receivable>(reference: Reference, message: M) {
    let Reference::Remote(id, node) = reference else {
        panic!("Can't send to a local alias!");
    };

    let message = serialize_value(&message);

    node_send_frame(Frame::from(Send::with_alias(id, message)), node);
}

/// Sends the given message to the remote node with the given name/node.
pub fn node_process_send_with_name<M: Receivable>(name: String, node: Node, message: M) {
    let message = serialize_value(&message);

    let node = node_register(node, false);

    node_send_frame(Frame::from(Send::with_name(name, message)), node);
}
