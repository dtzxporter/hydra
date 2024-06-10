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
use crate::ProcessInfo;
use crate::RuntimeInfo;

/// Unique registered name for the console server process.
const CONSOLE_NAME: &str = "$hydra_console";

/// Message used by the console server.
#[derive(Serialize, Deserialize)]
pub enum ConsoleServerMessage {
    ListNodes(NodeState),
    ListNodesSuccess(Vec<Node>),
    ListProcesses,
    ListProcessesSuccess(Vec<Pid>),
    ProcessesInfo(Vec<Pid>),
    ProcessesInfoSuccess(Vec<Option<ProcessInfo>>),
    ListRuntimeInfo,
    ListRuntimeInfoSuccess(RuntimeInfo),
}

/// Console acts as a relay to `hydra-console`. It collects and sends realtime information about the current hydra instance.
pub struct ConsoleServer {
    /// Used to prevent anyone from just constructing the console server, it's handled by applications.
    #[allow(unused)]
    _ignore: bool,
}

impl ConsoleServer {
    /// Constructs a new instance of [ConsoleServer].
    pub(super) const fn new() -> Self {
        Self { _ignore: true }
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

    /// Requests the list of running processes.
    pub async fn list_processes<T: Into<Dest>>(server: T) -> Result<Vec<Pid>, CallError> {
        use ConsoleServerMessage::*;

        match ConsoleServer::call(server.into(), ListProcesses, None).await? {
            ListProcessesSuccess(processes) => Ok(processes),
            _ => unreachable!(),
        }
    }

    /// Requests process info for the given processes.
    pub async fn processes_info<T: Into<Dest>>(
        server: T,
        processes: Vec<Pid>,
    ) -> Result<Vec<Option<ProcessInfo>>, CallError> {
        use ConsoleServerMessage::*;

        match ConsoleServer::call(server.into(), ProcessesInfo(processes), None).await? {
            ProcessesInfoSuccess(info) => Ok(info),
            _ => unreachable!(),
        }
    }

    /// Requests runtime info for the hydra instance.
    pub async fn runtime_info<T: Into<Dest>>(server: T) -> Result<RuntimeInfo, CallError> {
        use ConsoleServerMessage::*;

        match ConsoleServer::call(server.into(), ListRuntimeInfo, None).await? {
            ListRuntimeInfoSuccess(info) => Ok(info),
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

    async fn terminate(&mut self, reason: ExitReason) {
        #[cfg(feature = "tracing")]
        tracing::info!(console = ?Process::current(), reason = ?reason, "Console server has terminated");

        #[cfg(not(feature = "tracing"))]
        let _ = reason;
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        use ConsoleServerMessage::*;

        match message {
            ListNodes(state) => Ok(Some(ListNodesSuccess(Node::list_by_state(state)))),
            ListProcesses => Ok(Some(ListProcessesSuccess(Process::list()))),
            ProcessesInfo(pids) => {
                let process_info = pids.into_iter().map(Process::info).collect();

                Ok(Some(ProcessesInfoSuccess(process_info)))
            }
            ListRuntimeInfo => Ok(Some(ListRuntimeInfoSuccess(RuntimeInfo::load()))),
            _ => unreachable!(),
        }
    }
}
