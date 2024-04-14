use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;

use hydra::Message;
use hydra::Pid;
use hydra::Process;

static COUNTER: AtomicU64 = AtomicU64::new(0);

#[hydra::main]
async fn main() {
    let pid1 = Process::spawn(async {
        loop {
            let Message::User(pid2) = Process::receive::<Pid>().await else {
                panic!()
            };

            COUNTER.fetch_add(1, Ordering::Relaxed);

            Process::send(pid2, ());
        }
    });

    Process::spawn(async move {
        let pid2 = Process::current();

        loop {
            Process::send(pid1, pid2);
            let _ = Process::receive::<()>().await;
        }
    });

    let start = std::time::Instant::now();

    loop {
        Process::sleep(Duration::from_secs(1)).await;

        let elapsed = start.elapsed();
        let count = COUNTER.load(Ordering::Relaxed);

        let ops = count / elapsed.as_secs().max(1);

        println!("Msg/s: {}", ops);
    }
}
