use serde::Deserialize;
use serde::Serialize;

use crate::CallError;
use crate::Dest;
use crate::ExitReason;
use crate::From;
use crate::GenServer;
use crate::GenServerOptions;
use crate::Node;
use crate::NodeState;
use crate::Pid;
use crate::Process;

/// Unique registered name for the console server process.
const CONSOLE_NAME: &str = "$hydra_console";

/// Message used by the console server.
#[derive(Serialize, Deserialize)]
pub enum ConsoleServerMessage {
    ListNodes(NodeState),
    ListNodesSuccess(Vec<Node>),
}

/// Console acts as a relay to `hydra-console`. It collects and sends realtime information about the current hydra instance.
pub struct ConsoleServer {
    //
}

impl ConsoleServer {
    /// Constructs a new instance of [ConsoleServer].
    pub(super) const fn new() -> Self {
        Self {}
    }

    /// Starts a [ConsoleServer] process linked to the current process.
    pub(super) async fn start_link(self) -> Result<Pid, ExitReason> {
        GenServer::start_link(self, GenServerOptions::new().name(CONSOLE_NAME)).await
    }

    /// Requests the connected node list based on the state.
    pub async fn list_nodes<T: Into<Dest>>(
        server: T,
        state: NodeState,
    ) -> Result<Vec<Node>, CallError> {
        use ConsoleServerMessage::*;

        match ConsoleServer::call(server.into(), ListNodes(state), None).await? {
            ListNodesSuccess(nodes) => Ok(nodes),
            _ => unreachable!(),
        }
    }
}

impl GenServer for ConsoleServer {
    type Message = ConsoleServerMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        #[cfg(feature = "tracing")]
        tracing::info!(console = ?Process::current(), "Console server has started");

        Ok(())
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        use ConsoleServerMessage::*;

        match message {
            ListNodes(state) => Ok(Some(ListNodesSuccess(Node::list_by_state(state)))),
            _ => unreachable!(),
        }
    }
}
