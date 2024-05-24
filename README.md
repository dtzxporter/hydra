# <img src=".github/hydra.png" width="32" height="32"> Hydra
 
A framework for writing fault tolerant, highly scalable applications with the Rust programming language. It is:

- **Fast**: Native performance powered by Tokio's light weight task architecture.
- **Scalable**: All Hydra code runs inside lightweight threads of execution (called `Processes`) that are isolated and exchange information via messages.
- **Fault-Tolerant**: Inspired by Erlang/Elixir's OTP, Hydra provides many of the same concepts like `GenServer` and `Supervisor` to restart parts of your system when things to awry.
- **Distributed**: Hydra provides built-in support for running a fully distributed cluster of processes over a network of any size.

[![Crates.io][crates-badge]][crates-url]
[![Docs.rs][docs-badge]][docs-url]
[![MIT licensed][mit-badge]][mit-url]
[![Build Status][actions-badge]][actions-url]

[crates-badge]: https://img.shields.io/crates/v/hydra.svg
[crates-url]: https://crates.io/crates/hydra
[docs-badge]: https://img.shields.io/docsrs/hydra/latest
[docs-url]: https://docs.rs/hydra
[mit-badge]: https://img.shields.io/badge/license-MIT-blue.svg
[mit-url]: https://github.com/dtzxporter/hydra/blob/main/LICENSE
[actions-badge]: https://github.com/dtzxporter/hydra/workflows/CI/badge.svg
[actions-url]: https://github.com/dtzxporter/hydra/actions/workflows/ci.yml?query=branch%3Amain++

## Overview
Hydra runs on the [Tokio](https://github.com/tokio-rs/tokio) runtime, known for powering reliable, asynchronous, and slim applications with the Rust programming language. At a high level, Hydra provides the following major components:

- **Process**: A light weight task that supports sending and receiving messages.
- **GenServer**: A generic server process that provides request/reply and state management.
- **Supervisor**: A process which supervises other processes, used to provide fault-tolerance and encapsulate how our applications start and shutdown (gracefully).
- **Registry**: A process which acts as a centralized 'registry' of processes allowing you to lookup running processes by any key.
- **Node**: A mechanism to connect multiple nodes (instances) of Hydra together and monitor those connections.

## Example
A basic `GenServer` Stack application with Hydra.

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
use hydra::Application;
use hydra::ExitReason;
use hydra::From;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Pid;

use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize)]
enum StackMessage {
    Pop,
    PopResult(String),
    Push(String),
}

struct Stack {
    stack: Vec<String>,
}

impl Stack {
    pub fn with_entries(entries: Vec<&'static str>) -> Self {
        Self {
            stack: Vec::from_iter(entries.into_iter().map(Into::into)),
        }
    }
}

impl GenServer for Stack {
    type Message = StackMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        Ok(())
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        match message {
            StackMessage::Pop => Ok(Some(StackMessage::PopResult(self.stack.remove(0)))),
            _ => unreachable!(),
        }
    }

    async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
        match message {
            StackMessage::Push(value) => self.stack.insert(0, value),
            _ => unreachable!(),
        }
        Ok(())
    }
}

struct StackApplication;

impl Application for StackApplication {
    // Here, we must link a process for the application to monitor, usually, a Supervisor, but it can be any process.
    async fn start(&self) -> Result<Pid, ExitReason> {
        let pid = Stack::with_entries(vec!["hello", "world"])
            .start_link(GenServerOptions::new())
            .await
            .expect("Failed to start stack!");

        let result = Stack::call(pid, StackMessage::Pop, None)
            .await
            .expect("Stack call failed!");

        tracing::info!("{:?}", result);

        Stack::cast(pid, StackMessage::Push(String::from("rust")));

        let result = Stack::call(pid, StackMessage::Pop, None)
            .await
            .expect("Stack call failed!");

        tracing::info!("{:?}", result);

        // Otherwise, the application will run forever waiting for Stack to terminate.
        Stack::stop(pid, ExitReason::Normal, None).await?;

        Ok(pid)
    }
}

fn main() {
    // This method will return once the linked Process in StackApplication::start has terminated.
    Application::run(StackApplication)
}
```

Find more examples in the [examples](https://github.com/dtzxporter/hydra/tree/main/hydra/examples) folder. You can run them with: `cargo run --example=stack`.

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

## Benchmark
There is a message passing benchmark in the examples you can run using `cargo run --example=benchmark --release`. I've gotten the following results:

- **Intel 14700k**: 25902612 msg/s
- **Intel 7700k**: 7332666 msg/s
- **Apple M1 Max**: 5053229 msg/s

## License
This project is licensed under the [MIT license](https://github.com/dtzxporter/hydra/blob/main/LICENSE)

### Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in Hydra by you, shall be licensed as MIT, without any additional terms or conditions.