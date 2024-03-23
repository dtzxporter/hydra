use std::time::Duration;

use hydra::Message;
use hydra::Pid;
use hydra::Process;
use hydra::ProcessFlags;
use hydra::SystemMessage;

use hydra_platform::GenServer;
use hydra_platform::GenServerOptions;
use hydra_platform::Reply;

#[derive(Debug)]
enum MyMessage {
    Hello(String),
    Bye(Vec<u8>),
    Call(i32),
    Resp(i32),
}

struct MyServer;

impl GenServer for MyServer {
    type InitArg = ();
    type Message = MyMessage;

    async fn init(&mut self, _: Self::InitArg) {
        //
    }

    async fn handle_cast(&mut self, from: Pid, message: Self::Message) {
        println!(
            "My gen server got a cool message: from: {:?} {:?}",
            from, message
        );
    }

    async fn handle_call(
        &mut self,
        _reply: Reply<Self::Message>,
        message: Self::Message,
    ) -> Option<Self::Message> {
        match message {
            MyMessage::Call(start) => Some(MyMessage::Resp(start * 5)),
            _ => unimplemented!(),
        }
    }
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

        // Remove the monitor before we crash so we get nothing.

        // Send it.
        Process::send(us, MyMessage::Hello("wins".into()));

        let pid = MyServer.start_link((), GenServerOptions::new()).await;

        MyServer::cast(pid, MyMessage::Hello(String::from("yay")));

        let start = std::time::Instant::now();
        let _ = MyServer::call(pid, MyMessage::Call(20)).await;

        println!("Got call result:   {:?}", start.elapsed() / 100);

        loop {
            let recv = Process::receive::<MyMessage>().await;

            match recv {
                Message::User(MyMessage::Hello(string)) => {
                    println!("Got message: {:?}", string);
                }
                Message::User(MyMessage::Bye(bye)) => {
                    println!("Got bye: {:?}", bye);
                }
                Message::User(_) => {
                    unimplemented!()
                }
                Message::System(SystemMessage::Exit(from, exit_reason)) => {
                    println!("Got exit signal from: {:?} reason: {:?}", from, exit_reason);

                    Process::send(us, MyMessage::Bye("wins".as_bytes().to_vec()));
                }
                Message::System(SystemMessage::ProcessDown(from, monitor, exit_reason)) => {
                    println!(
                        "Got process down from: {:?} monitor: {:?} reason: {:?}",
                        from, monitor, exit_reason
                    );

                    Process::send(us, MyMessage::Bye("wins".as_bytes().to_vec()));
                }
            }
        }
    });

    tokio::time::sleep(Duration::from_secs(20)).await;
}
