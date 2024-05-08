# <img src=".github/hydra.png" width="32" height="32"> Hydra
 
A framework for writing fault tolerant, highly scalable applications with the Rust programming language. It is:

- **Fast**: Native performance powered by Tokio's light weight task architecture.
- **Scalable**: All Hydra code runs inside lightweight threads of execution (called `Processes`) that are isolated and exchange information via messages.
- **Fault-Tolerant**: Inspired by Erlang/Elixir's OTP, Hydra provides many of the same concepts like `Supervisor` and `GenServer` to restart parts of your system when things to awry.
- **Distributed**: Hydra provides built-in support for running a fully distributed cluster of processes over a network of any size.

[![Crates.io][crates-badge]][crates-url]
[![Docs.rs][docs-badge]][docs-url]
[![MIT licensed][mit-badge]][mit-url]

[crates-badge]: https://img.shields.io/crates/v/hydra.svg
[crates-url]: https://crates.io/crates/hydra
[docs-badge]: https://img.shields.io/docsrs/hydra/latest
[docs-url]: https://docs.rs/hydra
[mit-badge]: https://img.shields.io/badge/license-MIT-blue.svg
[mit-url]: https://github.com/dtzxporter/hydra/blob/main/LICENSE

## Overview
Hydra runs on the [Tokio](https://github.com/tokio-rs/tokio) runtime, known for powering reliable, asynchronous, and slim applications with the Rust programming language. At a high level, Hydra provides the following major components:

- **Process**: A light weight task that supports sending and receiving messages.
- **GenServer**: A generic server process that provides request/reply and state management.
- **Supervisor**: A process which supervises other processes, used to provide fault-tolerance and encapsulate how our applications start and shutdown (gracefully).
- **Node**: A mechanism to connect multiple nodes (instances) of Hydra together and monitor those connections.

## Example
A basic `GenServer`, `Supervisor` application with Hydra.

Make sure you have added Hydra, Serde in your Cargo.toml:
```toml
[dependencies]
hydra = "0.1.0"
serde = { version="1.0", features = "derive" }
```

Make sure that you are **not** aborting on panics, Hydra catches and manages panics for all `Processes`. Find and **remove** this line if it exists in your Cargo.toml:
```toml
panic = "abort"
```

Then, in your main.rs:
```rust
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
```

## Related Projects
The following projects are related to, or in use in Hydra such that it would be wise to learn them as well:

* [`tokio`]: A runtime for writing reliable, asynchronous, and slim applications with the Rust programming language.
* [`serde`]: A framework for serializing and deserializing Rust data structures efficiently and generically.
* [`tracing`]: A framework for application-level tracing and async-aware diagnostics.

[`tokio`]: https://github.com/tokio-rs/tokio
[`serde`]: https://github.com/serde-rs/serde
[`tracing`]: https://github.com/tokio-rs/tracing

## Changelog
[View Changelog](https://github.com/dtzxporter/hydra/blob/main/CHANGELOG.md)

## License
This project is licensed under the [MIT license](https://github.com/dtzxporter/hydra/blob/main/LICENSE)

### Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in Hydra by you, shall be licensed as MIT, without any additional terms or conditions.