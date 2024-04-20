use std::sync::Arc;

use tokio::net::TcpListener;

use crate::NodeOptions;
use crate::Pid;
use crate::Process;

pub struct NodeLocalSupervisor {
    pub name: String,
    pub options: NodeOptions,
    pub process: Pid,
}

async fn node_local_listener(supervisor: Arc<NodeLocalSupervisor>) {
    let listener = TcpListener::bind(supervisor.options.listen_address)
        .await
        .unwrap();

    loop {
        let Ok((socket, _)) = listener.accept().await else {
            continue;
        };

        // Spawn a process to handle this socket.
    }
}

pub async fn node_local_supervisor(name: String, options: NodeOptions) {
    let supervisor = Arc::new(NodeLocalSupervisor {
        name,
        options,
        process: Process::current(),
    });

    Process::spawn_link(node_local_listener(supervisor.clone()));
}
