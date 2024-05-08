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
    async fn start(&self) -> Result<hydra::Pid, ExitReason> {
        // Spawn two instances of `MyServer` with their own unique ids.
        let children = [
            MyServer::child_spec(()).id("server1"),
            MyServer::child_spec(()).id("server2"),
        ];

        // Restart only the terminated child.
        Supervisor::with_children(children)
            .strategy(SupervisionStrategy::OneForOne)
            .start_link(GenServerOptions::new())
            .await
    }
}

struct MyServer;

impl MyServer {
    /// A wrapper around the GenServer call "Hello".
    pub async fn hello<T: Into<Dest>>(server: T, string: &str) -> Result<String, CallError> {
        match MyServer::call(server, MyMessage::Hello(string.to_owned()), None).await? {
            MyMessage::HelloResponse(response) => Ok(response),
            _ => unreachable!(),
        }
    }
}

impl GenServer for MyServer {
    type InitArg = ();
    type Message = MyMessage;

    async fn init(&mut self, _init_arg: Self::InitArg) -> Result<(), ExitReason> {
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

    fn child_spec(init_arg: Self::InitArg) -> ChildSpec {
        ChildSpec::new("MyServer")
            .start(move || MyServer::start_link(MyServer, init_arg, GenServerOptions::new()))
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        match message {
            MyMessage::Hello(string) => {
                Ok(Some(MyMessage::HelloResponse(format!("{} world!", string))))
            }
            _ => unreachable!(),
        }
    }

    async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
        match message {
            MyMessage::Crash => {
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
