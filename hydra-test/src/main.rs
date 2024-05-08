use std::time::Duration;
use std::time::Instant;

use serde::Deserialize;
use serde::Serialize;

use hydra::ChildSpec;
use hydra::ExitReason;
use hydra::From;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Local;
use hydra::Process;
use hydra::SupervisionStrategy;
use hydra::Supervisor;

#[derive(Debug, Serialize, Deserialize)]
enum MyMessage {
    Hello(String),
    Bye(Vec<u8>, Local<Instant>),
    Call(i32),
    Resp(String),
    Crash,
}

struct MyServer;

impl GenServer for MyServer {
    type InitArg = ();
    type Message = MyMessage;

    async fn init(&mut self, _init_arg: Self::InitArg) -> Result<(), ExitReason> {
        let id = Process::current();

        Process::spawn(async move {
            Process::sleep(Duration::from_secs(1)).await;
            MyServer::cast(id, MyMessage::Crash);
        });

        Ok(())
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        match message {
            MyMessage::Hello(string) => Ok(Some(MyMessage::Resp(format!("{} world!", string)))),
            _ => Err(ExitReason::Kill),
        }
    }

    async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
        match message {
            MyMessage::Crash => {
                panic!("Whoops! We crashed!");
            }
            _ => Ok(()),
        }
    }
}

#[hydra::main]
async fn main() {
    let server = MyServer;
    let server = server
        .start((), GenServerOptions::new())
        .await
        .expect("Failed to start MyServer!");

    let children = [
        ChildSpec::new("server")
            .start(|| MyServer::start_link(MyServer, (), GenServerOptions::new())),
        ChildSpec::new("server2")
            .start(|| MyServer::start_link(MyServer, (), GenServerOptions::new())),
    ];

    let supervisor = Supervisor::with_children(children)
        .strategy(SupervisionStrategy::OneForOne)
        .start_link(GenServerOptions::new())
        .await
        .expect("Failed to start supervisor!");

    tracing::info!("Supervisor started: {:?}", supervisor);

    let start = std::time::Instant::now();

    for _ in 0..500 {
        let _response = MyServer::call(server, MyMessage::Hello(String::from("hello")), None)
            .await
            .expect("Failed to call MyServer!");
    }

    tracing::info!("Average req/reply latency: {:?}", start.elapsed() / 500);

    loop {
        let _ = Process::receive::<()>().await;
    }
}
