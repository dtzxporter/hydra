# Hydra Websockets

A websocket server library for the hydra framework.

[![Crates.io][crates-badge]][crates-url]
[![Docs.rs][docs-badge]][docs-url]
[![MIT licensed][mit-badge]][mit-url]
[![Build Status][actions-badge]][actions-url]

[crates-badge]: https://img.shields.io/crates/v/hydra-websockets.svg
[crates-url]: https://crates.io/crates/hydra-websockets
[docs-badge]: https://img.shields.io/docsrs/hydra-websockets/latest
[docs-url]: https://docs.rs/hydra-websockets
[mit-badge]: https://img.shields.io/badge/license-MIT-blue.svg
[mit-url]: https://github.com/dtzxporter/hydra/blob/main/LICENSE
[actions-badge]: https://github.com/dtzxporter/hydra/workflows/CI/badge.svg
[actions-url]: https://github.com/dtzxporter/hydra/actions/workflows/ci.yml?query=branch%3Amain++

## Example
A basic echo websocket server with Hydra.

Make sure you have added Hydra, Hydra Websockets in your Cargo.toml:
```toml
[dependencies]
hydra = "0.1"
hydra-websockets = "0.1"
```

Then, in your main.rs:
```rust
use std::net::SocketAddr;

use hydra::Application;
use hydra::ExitReason;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Pid;
use hydra::Process;

use hydra_websockets::WebsocketCommands;
use hydra_websockets::WebsocketHandler;
use hydra_websockets::WebsocketMessage;
use hydra_websockets::WebsocketRequest;
use hydra_websockets::WebsocketResponse;
use hydra_websockets::WebsocketServer;
use hydra_websockets::WebsocketServerConfig;

struct MyWebsocketHandler;

impl WebsocketHandler for MyWebsocketHandler {
    type Message = ();

    fn accept(
        _request: &WebsocketRequest,
        response: WebsocketResponse,
    ) -> Result<(WebsocketResponse, Self), ExitReason> {
        // You can extract any header information from `request` and pass it to the handler.
        Ok((response, MyWebsocketHandler))
    }

    async fn websocket_handle(
        &mut self,
        message: WebsocketMessage,
    ) -> Result<Option<WebsocketCommands>, ExitReason> {
        match message {
            WebsocketMessage::Text(text) => {
                tracing::info!(handler = ?Process::current(), message = ?text, "Got message");

                // Echo the command back to the client.
                Ok(Some(WebsocketCommands::with_send(text)))
            }
            _ => {
                // Hydra websockets automatically responds to ping requests.
                Ok(None)
            }
        }
    }
}

struct MyWebsocketApplication;

impl Application for MyWebsocketApplication {
    async fn start(&self) -> Result<Pid, ExitReason> {
        let address: SocketAddr = "127.0.0.1:1337".parse().unwrap();
        let config = WebsocketServerConfig::new(address);

        WebsocketServer::<MyWebsocketHandler>::new(config)
            .start_link(GenServerOptions::new())
            .await
    }
}

fn main() {
    Application::run(MyWebsocketApplication)
}
```

Find more examples in the [examples](https://github.com/dtzxporter/hydra/tree/main/hydra-websockets/examples) folder. You can run them with: `cargo run --example=server`.

## Changelog
[View Changelog](https://github.com/dtzxporter/hydra/blob/main/CHANGELOG.md)

## License
This project is licensed under the [MIT license](https://github.com/dtzxporter/hydra/blob/main/LICENSE)

### Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in Hydra by you, shall be licensed as MIT, without any additional terms or conditions.