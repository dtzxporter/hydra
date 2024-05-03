use std::time::Instant;

use serde::Deserialize;
use serde::Serialize;

use hydra::ChildSpec;
use hydra::ExitReason;
use hydra::From;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Local;
use hydra::SupervisionStrategy;
use hydra::Supervisor;

#[derive(Debug, Serialize, Deserialize)]
enum MyMessage {
    Hello(String),
    Bye(Vec<u8>, Local<Instant>),
    Call(i32),
    Resp(String),
}

struct MyServer;

impl GenServer for MyServer {
    type InitArg = ();
    type Message = MyMessage;

    async fn init(&mut self, _init_arg: Self::InitArg) -> Result<(), ExitReason> {
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
}

#[hydra::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let server = MyServer;
    let server = server
        .start_link((), GenServerOptions::new())
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
}
