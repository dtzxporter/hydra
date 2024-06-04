use std::sync::Arc;

use serde::Deserialize;
use serde::Serialize;

use tokio::net::TcpStream;

use tokio_util::codec::Framed;

use futures_util::stream;
use futures_util::stream::SplitSink;
use futures_util::stream::SplitStream;
use futures_util::SinkExt;
use futures_util::StreamExt;

use crate::frame::Codec;
use crate::frame::Frame;
use crate::frame::Hello;
use crate::frame::LinkDown;
use crate::frame::MonitorDown;
use crate::frame::MonitorUpdate;
use crate::frame::Ping;
use crate::frame::Pong;

use crate::link_create;
use crate::link_destroy;
use crate::monitor_create;
use crate::monitor_destroy;
use crate::node_accept;
use crate::node_forward_send;
use crate::node_link_destroy;
use crate::node_local_process;
use crate::node_process_link_create;
use crate::node_process_link_down;
use crate::node_process_monitor_cleanup;
use crate::node_process_monitor_destroy;
use crate::node_process_monitor_down;
use crate::node_register;
use crate::node_register_workers;
use crate::node_remote_supervisor_down;
use crate::node_send_frame;
use crate::process_exists_lock;
use crate::process_exit;
use crate::process_name_lookup;
use crate::process_sender;
use crate::ExitReason;
use crate::Local;
use crate::Message;
use crate::Node;
use crate::NodeLocalSupervisor;
use crate::NodeLocalSupervisorMessage;
use crate::Pid;
use crate::Process;
use crate::ProcessItem;
use crate::Reference;

type Reader = SplitStream<Framed<TcpStream, Codec>>;
type Writer = SplitSink<Framed<TcpStream, Codec>, Frame>;

#[derive(Serialize, Deserialize)]
pub enum NodeRemoteSenderMessage {
    /// Occurs when a new outbound frame is ready to be sent over the socket.
    SendFrame(Local<Frame>),
    /// Occurs when a bunch of new outbound frames are ready to be sent over the socket.
    SendFrames(Local<Vec<Frame>>),
}

#[derive(Serialize, Deserialize)]
pub enum NodeRemoteConnectorMessage {
    /// Occurs when the connector receives the local node supervisor information.
    LocalNodeSupervisor(Local<Arc<NodeLocalSupervisor>>),
}

#[derive(Serialize, Deserialize)]
enum NodeRemoteSupervisorMessage {
    /// Occurs when the receiver has been sent a ping frame, so we must respond with a pong frame.
    SendPong,
}

struct NodeRemoteSupervisor {
    node: Node,
    process: Pid,
    local_supervisor: Arc<NodeLocalSupervisor>,
}

struct NodeRemoteConnector {
    node: Node,
    process: Pid,
}

impl Drop for NodeRemoteSupervisor {
    fn drop(&mut self) {
        node_remote_supervisor_down(self.node.clone(), self.process);
    }
}

impl Drop for NodeRemoteConnector {
    fn drop(&mut self) {
        node_remote_supervisor_down(self.node.clone(), self.process);
    }
}

async fn node_remote_sender(mut writer: Writer, supervisor: Arc<NodeRemoteSupervisor>) {
    let send_timeout = supervisor.local_supervisor.options.heartbeat_interval;

    loop {
        let Ok(message) =
            Process::timeout(send_timeout, Process::receive::<NodeRemoteSenderMessage>()).await
        else {
            writer
                .send(Ping.into())
                .await
                .expect("Failed to send a message to the remote node!");
            continue;
        };

        match message {
            Message::User(NodeRemoteSenderMessage::SendFrame(frame)) => {
                writer
                    .send(frame.into_inner())
                    .await
                    .expect("Failed to send a message to the remote node!");
            }
            Message::User(NodeRemoteSenderMessage::SendFrames(frames)) => {
                let mut stream = stream::iter(frames.into_inner().into_iter().map(Ok));

                writer
                    .send_all(&mut stream)
                    .await
                    .expect("Failed to send multiple messages to the remote node!");
            }
            _ => unreachable!(),
        }
    }
}

async fn node_remote_receiver(mut reader: Reader, supervisor: Arc<NodeRemoteSupervisor>) {
    let recv_timeout = supervisor.local_supervisor.options.heartbeat_timeout;

    loop {
        let message = Process::timeout(recv_timeout, reader.next())
            .await
            .expect("Remote node timed out!")
            .expect("Remote node went down!")
            .expect("Failed to receive a message from the remote node!");

        match message {
            Frame::Hello(_) => unreachable!("Should never receive hello frame!"),
            Frame::Ping => {
                Process::send(supervisor.process, NodeRemoteSupervisorMessage::SendPong);
            }
            Frame::Pong => {
                // Maybe log this in metrics somewhere!
            }
            Frame::Send(send) => {
                node_forward_send(send);
            }
            Frame::Monitor(monitor) => {
                let node = node_register(supervisor.node.clone(), false);

                let from_id = monitor
                    .from_id
                    .expect("Must have a from_id for remote monitors!");

                let from = Pid::remote(from_id, node);
                let reference = Reference::remote(monitor.reference_id, node);

                if monitor.install {
                    if let Some(id) = monitor.process_id {
                        let process = Pid::local(id);

                        process_exists_lock(process, |exists| {
                            if exists {
                                monitor_create(process, reference, from, None);
                                node_process_monitor_cleanup(
                                    supervisor.node.clone(),
                                    reference,
                                    process,
                                );
                            } else {
                                let mut monitor_down = MonitorDown::new(ExitReason::from("noproc"));

                                monitor_down.monitors.push(reference.id());

                                node_send_frame(monitor_down.into(), node);
                            }
                        });
                    } else if let Some(name) = monitor.process_name {
                        if let Some(id) = process_name_lookup(&name) {
                            monitor_create(id, reference, from, None);

                            let monitor_update =
                                MonitorUpdate::new(from_id, id.id(), reference.id());

                            node_send_frame(monitor_update.into(), node);
                            node_process_monitor_cleanup(supervisor.node.clone(), reference, id);
                        } else {
                            let mut monitor_down = MonitorDown::new(ExitReason::from("noproc"));

                            monitor_down.monitors.push(reference.id());

                            node_send_frame(monitor_down.into(), node);
                        }
                    } else {
                        panic!("Must have either a process id or process name for monitor!");
                    };
                } else {
                    monitor_destroy(Pid::local(monitor.process_id.unwrap()), reference);

                    node_process_monitor_destroy(supervisor.node.clone(), reference);
                }
            }
            Frame::MonitorDown(monitor_down) => {
                for reference_id in monitor_down.monitors {
                    node_process_monitor_down(
                        supervisor.node.clone(),
                        Reference::local(reference_id),
                        monitor_down.exit_reason.clone(),
                    );
                }
            }
            Frame::MonitorUpdate(monitor_update) => {
                let node = node_register(supervisor.node.clone(), false);

                let reference = Reference::local(monitor_update.reference_id);
                let from = Pid::remote(monitor_update.from_id, node);

                process_sender(Pid::local(monitor_update.process_id))
                    .map(|sender| sender.send(ProcessItem::MonitorProcessUpdate(reference, from)));
            }
            Frame::Link(link) => {
                let node = node_register(supervisor.node.clone(), false);

                let process = Pid::local(link.process_id);
                let from = Pid::remote(link.from_id, node);

                if link.install {
                    if link_create(process, from, false) {
                        node_process_link_create(supervisor.node.clone(), from, process);
                    } else {
                        let mut link_down =
                            LinkDown::new(link.process_id, ExitReason::from("noproc"));

                        link_down.links.push(link.from_id);

                        node_send_frame(link_down.into(), node);
                    }
                } else {
                    link_destroy(process, from);

                    node_link_destroy(supervisor.node.clone(), from, process);
                }
            }
            Frame::LinkDown(link_down) => {
                let node = node_register(supervisor.node.clone(), false);

                let from = Pid::remote(link_down.from_id, node);

                for link in link_down.links {
                    node_process_link_down(
                        supervisor.node.clone(),
                        Pid::local(link),
                        from,
                        link_down.exit_reason.clone(),
                    );
                }
            }
            Frame::Exit(exit) => {
                let node = node_register(supervisor.node.clone(), false);

                let process = Pid::local(exit.process_id);
                let from = Pid::remote(exit.from_id, node);

                process_exit(process, from, exit.exit_reason);
            }
        }
    }
}

async fn node_remote_supervisor(
    writer: Writer,
    reader: Reader,
    hello: Hello,
    supervisor: Arc<NodeLocalSupervisor>,
) {
    let node = Node::from((hello.name, hello.broadcast_address));

    let supervisor = Arc::new(NodeRemoteSupervisor {
        node: node.clone(),
        process: Process::current(),
        local_supervisor: supervisor,
    });

    Process::link(supervisor.process);

    if !node_accept(node.clone(), Process::current()) {
        panic!("Not accepting node supervisor!");
    }

    let sender = Process::spawn_link(node_remote_sender(writer, supervisor.clone()));
    let receiver = Process::spawn_link(node_remote_receiver(reader, supervisor.clone()));

    node_register_workers(node, sender, receiver);

    loop {
        let message = Process::receive::<NodeRemoteSupervisorMessage>().await;

        match message {
            Message::User(NodeRemoteSupervisorMessage::SendPong) => {
                Process::send(
                    sender,
                    NodeRemoteSenderMessage::SendFrame(Local::new(Pong.into())),
                );
            }
            _ => unreachable!(),
        }
    }
}

pub async fn node_remote_accepter(socket: TcpStream, supervisor: Arc<NodeLocalSupervisor>) {
    if let Err(error) = socket.set_nodelay(true) {
        #[cfg(feature = "tracing")]
        tracing::warn!(error = ?error, "Failed to set TCP_NODELAY on socket");

        #[cfg(not(feature = "tracing"))]
        let _ = error;
    }

    let framed = Framed::new(socket, Codec::new());
    let (mut writer, mut reader) = framed.split();

    let hello = Hello::new(
        supervisor.name.clone(),
        supervisor.options.broadcast_address,
    );

    let handshake_timeout = supervisor.options.handshake_timeout;

    Process::timeout(handshake_timeout, writer.send(hello.into()))
        .await
        .expect("Timed out while sending hello handshake packet!")
        .expect("Failed to send hello handshake packet!");

    let frame = Process::timeout(handshake_timeout, reader.next())
        .await
        .expect("Timed out while receiving hello handshake packet!")
        .unwrap()
        .expect("Failed to receive hello handshake packet!");

    if let Frame::Hello(mut hello) = frame {
        if hello.validate() {
            Process::spawn(node_remote_supervisor(writer, reader, hello, supervisor));
            return;
        } else {
            panic!("Node handshake failed validation!");
        }
    }

    panic!("Received incorrect frame for node handshake!");
}

pub async fn node_remote_connector(node: Node) {
    let connector = NodeRemoteConnector {
        node: node.clone(),
        process: Process::current(),
    };

    let local = node_local_process().expect("Local node not started!");

    Process::link(local);

    Process::send(
        local,
        NodeLocalSupervisorMessage::RequestLocalSupervisor(Process::current()),
    );

    let Message::User(NodeRemoteConnectorMessage::LocalNodeSupervisor(supervisor)) =
        Process::receive::<NodeRemoteConnectorMessage>().await
    else {
        panic!("Received unexpected message in remote connector!");
    };

    let address = node
        .address()
        .expect("Must have an address for a remote node!");

    let handshake_timeout = supervisor.options.handshake_timeout;

    let socket = Process::timeout(handshake_timeout, TcpStream::connect(address))
        .await
        .expect("Timed out while connecting to the node!")
        .expect("Failed to connect to the node!");

    if let Err(error) = socket.set_nodelay(true) {
        #[cfg(feature = "tracing")]
        tracing::warn!(error = ?error, "Failed to set TCP_NODELAY on socket");

        #[cfg(not(feature = "tracing"))]
        let _ = error;
    }

    let framed = Framed::new(socket, Codec::new());
    let (mut writer, mut reader) = framed.split();

    let hello = Hello::new(
        supervisor.name.clone(),
        supervisor.options.broadcast_address,
    );

    Process::timeout(handshake_timeout, writer.send(hello.into()))
        .await
        .expect("Timed out while sending hello handshake packet!")
        .expect("Failed to send hello handshake packet!");

    let frame = Process::timeout(handshake_timeout, reader.next())
        .await
        .expect("Timed out while receiving hello handshake packet!")
        .unwrap()
        .expect("Failed to receive hello handshake packet!");

    if let Frame::Hello(mut hello) = frame {
        let matches = node == (hello.name.as_str(), hello.broadcast_address);

        if hello.validate() && matches {
            std::mem::forget(connector);
            return node_remote_supervisor(writer, reader, hello, supervisor.into_inner()).await;
        } else if !matches {
            panic!(
                "Node was not the expected node: {:?} (wanted) {:?} (received).",
                node,
                Node::from((hello.name, hello.broadcast_address))
            );
        } else {
            panic!("Node handshake failed validation!");
        }
    }

    panic!("Received incorrect frame for node handshake!");
}
