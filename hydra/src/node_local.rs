use std::sync::Arc;

use serde::Deserialize;
use serde::Serialize;

use tokio::net::TcpListener;

use crate::node_remote_accepter;
use crate::Local;
use crate::Message;
use crate::NodeOptions;
use crate::NodeRemoteConnectorMessage;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;

#[derive(Serialize, Deserialize)]
pub enum NodeLocalSupervisorMessage {
    /// Occurs when a connector process requests the supervisor.
    RequestLocalSupervisor(Pid),
}

pub struct NodeLocalSupervisor {
    pub name: String,
    pub options: NodeOptions,
    pub process: Pid,
}

impl Drop for NodeLocalSupervisor {
    fn drop(&mut self) {
        // We need to clean up this node!
        unimplemented!()
    }
}

async fn node_local_listener(supervisor: Arc<NodeLocalSupervisor>) {
    let listener = TcpListener::bind(supervisor.options.listen_address)
        .await
        .expect("Failed to bind socket for local node listener!");

    loop {
        let Ok((socket, _)) = listener.accept().await else {
            continue;
        };

        Process::spawn(node_remote_accepter(socket, supervisor.clone()));
    }
}

pub async fn node_local_supervisor(name: String, options: NodeOptions) {
    Process::set_flags(ProcessFlags::TRAP_EXIT);

    let supervisor = Arc::new(NodeLocalSupervisor {
        name,
        options,
        process: Process::current(),
    });

    let listener = Process::spawn_link(node_local_listener(supervisor.clone()));

    loop {
        let message = Process::receive::<NodeLocalSupervisorMessage>().await;

        match message {
            Message::User(NodeLocalSupervisorMessage::RequestLocalSupervisor(process)) => {
                let supervisor = Local::new(supervisor.clone());

                Process::send(
                    process,
                    NodeRemoteConnectorMessage::LocalNodeSupervisor(supervisor),
                );
            }
            Message::System(SystemMessage::Exit(pid, exit_reason)) => {
                if pid == listener {
                    panic!("Lost the local node listener: {:?}!", exit_reason);
                }
            }
            _ => unreachable!(),
        }
    }
}
