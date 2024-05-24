use std::net::SocketAddr;

use hydra::Application;
use hydra::ExitReason;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Pid;
use hydra::Process;

use hydra_websockets::Request;
use hydra_websockets::Response;
use hydra_websockets::WebsocketCommand;
use hydra_websockets::WebsocketHandler;
use hydra_websockets::WebsocketMessage;
use hydra_websockets::WebsocketServer;
use hydra_websockets::WebsocketServerConfig;

struct MyWebsocketHandler;

impl WebsocketHandler for MyWebsocketHandler {
    type Message = ();

    fn accept(_request: &Request, response: Response) -> Result<(Response, Self), ExitReason> {
        // You can extract any header information from `request` and pass it to the handler.
        Ok((response, MyWebsocketHandler))
    }

    async fn websocket_handle(
        &mut self,
        message: WebsocketMessage,
    ) -> Result<Vec<WebsocketCommand>, ExitReason> {
        match message {
            WebsocketMessage::Text(text) => {
                tracing::info!(handler = ?Process::current(), message = ?text, "Got message");

                // Echo the command back to the client.
                Ok(vec![WebsocketCommand::send(text)])
            }
            _ => {
                // Hydra websockets automatically responds to ping requests.
                Ok(vec![])
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
