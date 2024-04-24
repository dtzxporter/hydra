use std::net::SocketAddr;
use std::time::Duration;

use hydra::Node;
use hydra::NodeOptions;
use hydra::Process;

#[hydra::main]
async fn main() {
    Node::start(
        "hydra-test-client",
        NodeOptions::new()
            .listen_address("127.0.0.1:1338".parse::<SocketAddr>().unwrap())
            .broadcast_address("127.0.0.1:1338".parse::<SocketAddr>().unwrap()),
    );

    let address: SocketAddr = "127.0.0.1:1337".parse().unwrap();

    Node::connect(("hydra-test-main", address));

    Process::sleep(Duration::from_secs(5)).await;

    println!("Nodes: {:?}", Node::list());

    Process::sleep(Duration::from_secs(1000)).await;
}
