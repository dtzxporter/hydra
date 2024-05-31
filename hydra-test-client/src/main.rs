use std::net::SocketAddr;
use std::time::Duration;

use hydra::Message;
use hydra::Node;
use hydra::NodeOptions;
use hydra::Pid;
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

    Node::monitor(("hydra-test-main", address));

    Process::sleep(Duration::from_secs(5)).await;

    println!("Nodes: {:?}", Node::list());

    for i in 0..8 {
        Process::spawn(async move {
            let pid = Process::current();
            let mut pid2: Option<Pid> = None;

            loop {
                if let Some(pid2) = pid2 {
                    Process::send(pid2, pid);
                } else {
                    Process::send(
                        (format!("bench-receive{}", i), ("hydra-test-main", address)),
                        pid,
                    );
                }

                let pidd = Process::receive::<Pid>().await;

                if let Message::User(pidd) = pidd {
                    pid2 = Some(pidd);
                }
            }
        });
    }

    Process::sleep(Duration::from_secs(1000)).await;
}
