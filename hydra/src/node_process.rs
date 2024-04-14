use std::sync::Arc;

use serde::Deserialize;
use serde::Serialize;

use tokio::net::TcpListener;
use tokio::net::TcpStream;

use tokio_util::codec::Framed;

use futures_util::stream::SplitSink;
use futures_util::stream::SplitStream;
use futures_util::SinkExt;
use futures_util::StreamExt;

use crate::frame::Codec;
use crate::frame::Frame;
use crate::frame::Hello;

use crate::Message;
use crate::NodeOptions;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;
use crate::NODE_REGISTRY;

type Reader = SplitStream<Framed<TcpStream, Codec>>;
type Writer = SplitSink<Framed<TcpStream, Codec>, Frame>;

#[derive(Serialize, Deserialize)]
pub enum NodeMessage {
    Connect(Pid),
}

struct NodeInformation {
    name: String,
    options: NodeOptions,
}

async fn local_node_process(info: Arc<NodeInformation>) {
    let listener = TcpListener::bind(info.options.listen_address)
        .await
        .unwrap();

    loop {
        let Ok((socket, _)) = listener.accept().await else {
            continue;
        };

        Process::spawn(remote_node_process(socket, info.clone()));
    }
}

async fn remote_node_send_process(_writer: Writer) {
    // I send messages to the writer.
}

async fn remote_node_handshake(
    writer: &mut Writer,
    reader: &mut Reader,
    info: Arc<NodeInformation>,
) -> Option<Hello> {
    let hello = Hello::new(info.name.clone(), info.options.broadcast_address);

    // Send our hello packet to inform the other end of who we're identifying as.
    writer.send(hello.into()).await.ok()?;

    // Wait for the remote node's hello packet, which must be the first packet sent.
    if let Frame::Hello(hello) = reader.next().await?.ok()? {
        if !hello.validate() {
            return None;
        } else {
            return Some(hello);
        }
    }

    // We received the wrong frame.
    None
}

async fn remote_node_process(socket: TcpStream, info: Arc<NodeInformation>) {
    let framed = Framed::new(socket, Codec::new());
    let (mut writer, mut reader) = framed.split();

    match remote_node_handshake(&mut writer, &mut reader, info.clone()).await {
        Some(_hello) => {
            // Make sure this process is registered to handle this node information, otherwise
            // Exit if a process is already registered to handle this node.
        }
        None => {
            // Exit, could we be registered here?
            return;
        }
    }

    // Send our data.
    // Wait for their data...

    // Do the initial handshake before spawning read/writer processes?

    // Register ourself as the handler for this node.
    // Then forward packets to reader/writer nodes.
    Process::spawn_link(remote_node_send_process(writer));
}

pub async fn start_local_node(name: String, options: NodeOptions) {
    // Make sure we can gracefully clean up the local node registration
    // in the event that the local node process has an error.
    Process::set_flags(ProcessFlags::TRAP_EXIT);

    let info = Arc::new(NodeInformation { name, options });

    let listener = Process::spawn_link(local_node_process(info.clone()));

    loop {
        let message: Message<i32> = Process::receiver().receive().await;

        match message {
            Message::User(_) => {
                unimplemented!()
            }
            Message::System(SystemMessage::Exit(from, _)) => {
                if from == listener {
                    break;
                }
            }
            Message::System(_) => {
                unimplemented!()
            }
        }
    }

    NODE_REGISTRY.write().unwrap().remove(&0);
}
