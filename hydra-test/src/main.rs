use std::time::Duration;

use serde::Deserialize;
use serde::Serialize;

use hydra::Application;
use hydra::CallError;
use hydra::ChildSpec;
use hydra::Dest;
use hydra::ExitReason;
use hydra::From;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Pid;
use hydra::Process;
use hydra::SupervisionStrategy;
use hydra::Supervisor;

#[derive(Debug, Serialize, Deserialize)]
enum MyMessage {
    Hello(String),
    HelloResponse(String),
    Crash,
}

struct MyApplication;

impl Application for MyApplication {
    async fn start(&self) -> Result<Pid, ExitReason> {
        // Spawn two instances of `MyServer` with their own unique ids.
        let children = [
            MyServer::new().child_spec().id("server1"),
            MyServer::new().child_spec().id("server2"),
        ];

        // Restart only the terminated child.
        Supervisor::with_children(children)
            .strategy(SupervisionStrategy::OneForOne)
            .start_link(GenServerOptions::new())
            .await
    }
}

#[derive(Clone)]
struct MyServer;

impl MyServer {
    /// Constructs a new [MyServer].
    pub fn new() -> Self {
        Self
    }

    /// A wrapper around the GenServer call "Hello".
    pub async fn hello<T: Into<Dest>>(server: T, string: &str) -> Result<String, CallError> {
        use MyMessage::*;

        match MyServer::call(server, Hello(string.to_owned()), None).await? {
            HelloResponse(response) => Ok(response),
            _ => unreachable!(),
        }
    }

    /// Builds the child specification for [MyServer].
    pub fn child_spec(self) -> ChildSpec {
        ChildSpec::new("MyServer")
            .start(move || MyServer::start_link(MyServer, GenServerOptions::new()))
    }
}

impl GenServer for MyServer {
    type Message = MyMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        let server = Process::current();

        Process::spawn(async move {
            // Ask for a formatted string.
            let hello_world = MyServer::hello(server, "hello")
                .await
                .expect("Failed to call server!");

            tracing::info!("Got: {:?}", hello_world);

            // Wait before crashing.
            Process::sleep(Duration::from_secs(1)).await;

            // Crash the process so the supervisor restarts it.
            MyServer::cast(server, MyMessage::Crash);
        });

        Ok(())
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        use MyMessage::*;

        match message {
            Hello(string) => Ok(Some(HelloResponse(format!("{} world!", string)))),
            _ => unreachable!(),
        }
    }

    async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
        use MyMessage::*;

        match message {
            Crash => {
                panic!("Whoops! We crashed!");
            }
            _ => unreachable!(),
        }
    }
}

fn main() {
    // This method will only return once the supervisor linked in `start` has terminated.
    Application::run(MyApplication)
}
