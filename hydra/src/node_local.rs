use std::sync::Arc;

use tokio::net::TcpListener;

use crate::node_remote_accepter;
use crate::NodeOptions;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;

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

    Process::spawn_link(node_local_listener(supervisor.clone()));
}
