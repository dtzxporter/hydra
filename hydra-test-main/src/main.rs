use std::net::SocketAddr;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use hydra::Message;
use hydra::Node;
use hydra::NodeOptions;
use hydra::Pid;
use hydra::Process;

static COUNTER: AtomicU64 = AtomicU64::new(0);

#[hydra::main]
async fn main() {
    Node::start(
        "hydra-test-main",
        NodeOptions::new()
            .listen_address("127.0.0.1:1337".parse::<SocketAddr>().unwrap())
            .broadcast_address("127.0.0.1:1337".parse::<SocketAddr>().unwrap()),
    );

    for i in 0..8 {
        Process::spawn(async move {
            Process::register(Process::current(), format!("bench-receive{}", i))
                .expect("Failed to register process!");

            let pid = Process::current();

            loop {
                let Message::User(pid2) = Process::receive::<Pid>().await else {
                    panic!()
                };

                COUNTER.fetch_add(1, Ordering::Relaxed);

                Process::send(pid2, pid);
            }
        });
    }

    let mut start = Instant::now();

    loop {
        Process::sleep(Duration::from_secs(1)).await;

        let elapsed = start.elapsed();
        let count = COUNTER.load(Ordering::Relaxed);

        if count == 0 {
            tracing::info!("Waiting for first message...");
            start = Instant::now();
            continue;
        }

        let ops = count / elapsed.as_secs().max(1);

        tracing::info!("Msg/s: {}", ops);
    }
}
