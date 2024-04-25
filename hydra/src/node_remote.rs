use std::sync::Arc;

use serde::Deserialize;
use serde::Serialize;

use tokio::net::TcpStream;

use tokio_util::codec::Framed;

use pingora_timeout::timeout;

use futures_util::stream;
use futures_util::stream::SplitSink;
use futures_util::stream::SplitStream;
use futures_util::SinkExt;
use futures_util::StreamExt;

use crate::frame::Codec;
use crate::frame::Frame;
use crate::frame::Hello;
use crate::frame::Ping;
use crate::frame::Pong;

use crate::node_accept;
use crate::node_forward_send;
use crate::node_local_process;
use crate::node_remote_supervisor_down;
use crate::node_set_send_recv;
use crate::Local;
use crate::Message;
use crate::Node;
use crate::NodeLocalSupervisor;
use crate::NodeLocalSupervisorMessage;
use crate::Pid;
use crate::Process;

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
            timeout(send_timeout, Process::receive::<NodeRemoteSenderMessage>()).await
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
        let message = timeout(recv_timeout, reader.next())
            .await
            .expect("Remote node timed out!")
            .unwrap()
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
    };

    let sender = Process::spawn_link(node_remote_sender(writer, supervisor.clone()));
    let receiver = Process::spawn_link(node_remote_receiver(reader, supervisor.clone()));

    node_set_send_recv(node, sender, receiver);

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
    let framed = Framed::new(socket, Codec::new());
    let (mut writer, mut reader) = framed.split();

    let hello = Hello::new(
        supervisor.name.clone(),
        supervisor.options.broadcast_address,
    );

    let handshake_timeout = supervisor.options.handshake_timeout;

    timeout(handshake_timeout, writer.send(hello.into()))
        .await
        .expect("Timed out while sending hello handshake packet!")
        .expect("Failed to send hello handshake packet!");

    let frame = timeout(handshake_timeout, reader.next())
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

    let socket = timeout(handshake_timeout, TcpStream::connect(address))
        .await
        .expect("Timed out while connecting to the node!")
        .expect("Failed to connect to the node!");

    let framed = Framed::new(socket, Codec::new());
    let (mut writer, mut reader) = framed.split();

    let hello = Hello::new(
        supervisor.name.clone(),
        supervisor.options.broadcast_address,
    );

    timeout(handshake_timeout, writer.send(hello.into()))
        .await
        .expect("Timed out while sending hello handshake packet!")
        .expect("Failed to send hello handshake packet!");

    let frame = timeout(handshake_timeout, reader.next())
        .await
        .expect("Timed out while receiving hello handshake packet!")
        .unwrap()
        .expect("Failed to receive hello handshake packet!");

    if let Frame::Hello(mut hello) = frame {
        if hello.validate() {
            std::mem::forget(connector);
            return node_remote_supervisor(writer, reader, hello, supervisor.into_inner()).await;
        } else {
            panic!("Node handshake failed validation!");
        }
    }

    panic!("Received incorrect frame for node handshake!");
}
