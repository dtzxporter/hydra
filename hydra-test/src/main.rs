use std::time::Duration;

use hydra::Message;
use hydra::Process;
use hydra::ProcessFlags;
use hydra::SystemMessage;

enum MyMessage {
    Hello(String),
}

#[tokio::main]
async fn main() {
    Process::spawn(async move {
        // This will prevent our death pact from killing us.
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        let us = Process::current();

        println!("Process: {:?} running...", us);

        // Links this process to the parent, a (death pact).
        Process::spawn_link(async move {
            panic!("We're going down: {:?}!", Process::current());
        });

        // Send it.
        Process::send(us, MyMessage::Hello("wins".into()));

        loop {
            let recv = Process::receive::<MyMessage>().await;

            match recv {
                Message::User(MyMessage::Hello(string)) => {
                    println!("Got message: {:?}", string);
                }
                Message::System(SystemMessage::Exit(from, exit_reason)) => {
                    println!("Got exit signal from: {:?} reason: {:?}", from, exit_reason);
                }
            }
        }
    });

    tokio::time::sleep(Duration::from_secs(20)).await;
}
