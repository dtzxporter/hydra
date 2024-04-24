use std::net::SocketAddr;
use std::time::Duration;

use hydra::Node;
use hydra::NodeOptions;
use hydra::Process;

#[hydra::main]
async fn main() {
    Node::start(
        "hydra-test-main",
        NodeOptions::new()
            .listen_address("127.0.0.1:1337".parse::<SocketAddr>().unwrap())
            .broadcast_address("127.0.0.1:1337".parse::<SocketAddr>().unwrap()),
    );

    Process::sleep(Duration::from_secs(1000)).await;
}
