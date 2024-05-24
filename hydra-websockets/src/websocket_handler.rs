use std::future::Future;

use tokio_tungstenite::tungstenite;

use tungstenite::handshake::server::Request;
use tungstenite::handshake::server::Response;

use hydra::ExitReason;
use hydra::Message;
use hydra::Pid;
use hydra::Receivable;

/// A process that handles websocket messages.
pub trait WebsocketHandler
where
    Self: Sized,
{
    type Message: Receivable;

    // Take it and run away.
    fn accept(request: &Request, response: Response) -> Result<(Response, Self), ExitReason>;

    fn websocket_info(
        &mut self,
        info: Message<Self::Message>,
    ) -> impl Future<Output = Result<(), ExitReason>> + Send {
        async move {
            let _ = info;

            Ok(())
        }
    }
}

pub(crate) async fn start_websocket_handler<T: WebsocketHandler>(handler: T) {
    panic!()
}
