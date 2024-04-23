use std::sync::Arc;

use tokio::net::TcpListener;

use crate::node_remote_accepter;
use crate::Message;
use crate::NodeOptions;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;

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
        .unwrap();

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
        let message = Process::receive::<()>().await;

        match message {
            Message::System(SystemMessage::Exit(pid, _)) => {
                if pid == listener {
                    panic!("Lost the local node listener!");
                }
            }
            _ => unreachable!(),
        }
    }
}
