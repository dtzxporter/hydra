# <img src=".github/hydra.png" width="32" height="32"> Hydra
 
A framework for writing fault tolerant, highly scalable applications with the Rust programming language. It is:

- **Fast**: Native performance powered by Tokio's light weight task architecture.
- **Scalable**: All Hydra code runs inside lightweight threads of execution (called `Processes`) that are isolated and exchange information via messages.
- **Fault-Tolerant**: Inspired by Erlang/Elixir's OTP, Hydra provides many of the same concepts like `Supervisor` and `GenServer` to restart parts of your system when things to awry.
- **Distributed**: Hydra provides built-in support for running a fully distributed cluster of processes over a network of any size.

[![Crates.io][crates-badge]][crates-url]
[![MIT licensed][mit-badge]][mit-url]

[crates-badge]: https://img.shields.io/crates/v/hydra.svg
[crates-url]: https://crates.io/crates/hydra
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

Make sure you have added Hydra in your Cargo.toml:
```toml
[dependencies]
hydra = "0.1.0"
```

Make sure that you are **not** aborting on panics, Hydra catches and manages panics for all `Processes`. Find and **remove** this line if it exists in your Cargo.toml:
```toml
panic = "abort"
```

Then, in your main.rs:
```rust
#[hydra::main]
async fn main() {
    //
}
```

## Changelog
[View Changelog](https://github.com/dtzxporter/hydra/blob/main)


## License
This project is licensed under the [MIT license](https://github.com/dtzxporter/hydra/blob/main/LICENSE)

### Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in Hydra by you, shall be licensed as MIT, without any additional terms or conditions.