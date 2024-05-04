use std::collections::BTreeSet;
use std::pin::Pin;
use std::time::Duration;
use std::time::Instant;

use serde::Deserialize;
use serde::Serialize;

use crate::AutoShutdown;
use crate::CallError;
use crate::ChildSpec;
use crate::ChildType;
use crate::Dest;
use crate::ExitReason;
use crate::From;
use crate::GenServer;
use crate::GenServerOptions;
use crate::Message;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::Reference;
use crate::Restart;
use crate::Shutdown;
use crate::SystemMessage;

/// A supervision child.
struct SupervisedChild {
    spec: ChildSpec,
    pid: Option<Pid>,
}

/// A supervisor message.
#[doc(hidden)]
#[derive(Debug, Serialize, Deserialize)]
pub enum SupervisorMessage {
    TryAgainRestartPid(Pid),
    TryAgainRestartId(String),
    CountChildrenRequest,
    CountChildrenResponse(SupervisorCounts),
}

/// Contains the counts of all of the supervised children.
#[derive(Debug, Serialize, Deserialize)]
pub struct SupervisorCounts {
    /// The total count of children, dead or alive.
    pub specs: usize,
    /// The count of all actively running child processes managed by this supervisor.
    pub active: usize,
    /// The count of all children marked as `supervisor` dead or alive.
    pub supervisors: usize,
    /// The count of all children marked as `worker` dead or alive.
    pub workers: usize,
}

/// The supervision strategy to use for each child.
#[derive(Debug, Clone, Copy)]
pub enum SupervisionStrategy {
    /// If a child process terminates, only that process is restarted.
    OneForOne,
    /// If a child process terminates, all other child processes are terminated and then all child processes are restarted.
    OneForAll,
    /// If a child process terminates, the terminated child process and the rest of the children started after it, are terminated and restarted.
    RestForOne,
}

/// A supervisor is a process which supervises other processes, which we refer to as child processes.
/// Supervisors are used to build a hierarchical process structure called a supervision tree.
/// Supervision trees provide fault-tolerance and encapsulate how our applications start and shutdown.
pub struct Supervisor {
    children: Vec<SupervisedChild>,
    identifiers: BTreeSet<String>,
    restarts: Vec<Instant>,
    strategy: SupervisionStrategy,
    auto_shutdown: AutoShutdown,
    max_restarts: usize,
    max_duration: Duration,
}

impl Supervisor {
    /// Constructs a new instance of [Supervisor] with no children.
    pub const fn new() -> Self {
        Self {
            children: Vec::new(),
            identifiers: BTreeSet::new(),
            restarts: Vec::new(),
            strategy: SupervisionStrategy::OneForOne,
            auto_shutdown: AutoShutdown::Never,
            max_restarts: 3,
            max_duration: Duration::from_secs(5),
        }
    }

    /// Constructs a new instance of [Supervisor] with the given children.
    pub fn with_children<T: IntoIterator<Item = ChildSpec>>(children: T) -> Self {
        let mut result = Self::new();

        for child in children {
            result = result.add_child(child);
        }

        result
    }

    /// Adds a child to this [Supervisor].
    pub fn add_child(mut self, child: ChildSpec) -> Self {
        if self.identifiers.contains(&child.id) {
            panic!("Child id was not unique!");
        }

        self.identifiers.insert(child.id.clone());

        self.children.push(SupervisedChild {
            spec: child,
            pid: None,
        });

        self
    }

    /// Sets the supervision strategy for the [Supervisor].
    pub const fn strategy(mut self, strategy: SupervisionStrategy) -> Self {
        self.strategy = strategy;
        self
    }

    /// Sets the behavior to use when a significant process exits.
    pub const fn auto_shutdown(mut self, auto_shutdown: AutoShutdown) -> Self {
        self.auto_shutdown = auto_shutdown;
        self
    }

    /// Sets the maximum number of restarts allowed in a time frame.
    ///
    /// Defaults to 3.
    pub const fn max_restarts(mut self, max_restarts: usize) -> Self {
        self.max_restarts = max_restarts;
        self
    }

    /// Sets the time frame in which `max_restarts` applies.
    ///
    /// Defaults to 5s.
    pub const fn max_duration(mut self, max_duration: Duration) -> Self {
        self.max_duration = max_duration;
        self
    }

    /// Creates a supervisor process as part of a supervision tree.
    ///
    /// For example, this function ensures that the supervisor is linked to the calling process (its supervisor).
    ///
    /// This will not return until all of the child processes have been started.
    pub async fn start_link(self, options: GenServerOptions) -> Result<Pid, ExitReason> {
        GenServer::start_link(self, (), options).await
    }

    /// Returns [SupervisorCounts] containing the counts for each of the different child specifications.
    pub async fn count_children<T: Into<Dest>>(server: T) -> Result<SupervisorCounts, CallError> {
        match Supervisor::call(server, SupervisorMessage::CountChildrenRequest, None).await? {
            SupervisorMessage::CountChildrenResponse(counts) => Ok(counts),
            _ => unreachable!(),
        }
    }

    /// Starts all of the children.
    async fn start_children(&mut self) -> Result<(), ExitReason> {
        let mut remove: Vec<usize> = Vec::new();

        for index in 0..self.children.len() {
            match self.start_child(index).await {
                Ok(pid) => {
                    let child = &mut self.children[index];

                    child.pid = pid;

                    if child.is_temporary() && pid.is_none() {
                        remove.push(index);
                    }
                }
                Err(reason) => {
                    #[cfg(feature = "tracing")]
                    tracing::error!(reason = ?reason, child_id = ?self.children[index].spec.id, "Start error.");

                    return Err(ExitReason::from("failed_to_start_child"));
                }
            }
        }

        for index in remove.into_iter().rev() {
            self.remove_child(index);
        }

        Ok(())
    }

    /// Terminates all of the children.
    async fn terminate_children(&mut self) {
        let mut remove: Vec<usize> = Vec::new();

        for (index, child) in self.children.iter_mut().enumerate().rev() {
            if child.is_temporary() {
                remove.push(index);
            }

            let Some(pid) = child.pid.take() else {
                continue;
            };

            if let Err(reason) = shutdown(pid, child.shutdown()).await {
                #[cfg(feature = "tracing")]
                tracing::error!(reason = ?reason, child_pid = ?pid, "Shutdown error.");

                #[cfg(not(feature = "tracing"))]
                let _ = reason;
            }
        }

        for index in remove {
            self.remove_child(index);
        }
    }

    /// Terminates a single child.
    async fn terminate_child(&mut self, index: usize) {
        let child = &mut self.children[index];

        let Some(pid) = child.pid.take() else {
            return;
        };

        let _ = shutdown(pid, child.shutdown()).await;
    }

    /// Checks all of the children for correct specification and then starts them.
    async fn init_children(&mut self) -> Result<(), ExitReason> {
        if let Err(reason) = self.start_children().await {
            self.terminate_children().await;

            return Err(reason);
        }

        Ok(())
    }

    /// Restarts a child that exited for the given `reason`.
    async fn restart_child(&mut self, pid: Pid, reason: ExitReason) -> Result<(), ExitReason> {
        let Some(index) = self.find_child(pid) else {
            return Ok(());
        };

        let child = &mut self.children[index];

        // Permanent children are always restarted.
        if child.is_permanent() {
            #[cfg(feature = "tracing")]
            tracing::error!(reason = ?reason, child_id = ?child.spec.id, child_pid = ?child.pid, "Child terminated.");

            if self.add_restart() {
                return Err(ExitReason::from("shutdown"));
            }

            self.restart(index).await;

            return Ok(());
        }

        // If it's not permanent, check if it's a normal reason.
        if reason.is_normal() || reason == "shutdown" {
            let child = self.remove_child(index);

            if self.check_auto_shutdown(child) {
                return Err(ExitReason::from("shutdown"));
            } else {
                return Ok(());
            }
        }

        // Not a normal reason, check if transient.
        if child.is_transient() {
            #[cfg(feature = "tracing")]
            tracing::error!(reason = ?reason, child_id = ?child.spec.id, child_pid = ?child.pid, "Child terminated.");

            if self.add_restart() {
                return Err(ExitReason::from("shutdown"));
            }

            self.restart(index).await;

            return Ok(());
        }

        // Not transient, check if temporary and clean up.
        if child.is_temporary() {
            #[cfg(feature = "tracing")]
            tracing::error!(reason = ?reason, child_id = ?child.spec.id, child_pid = ?child.pid, "Child terminated.");

            let child = self.remove_child(index);

            if self.check_auto_shutdown(child) {
                return Err(ExitReason::from("shutdown"));
            }
        }

        Ok(())
    }

    /// Restarts one or more children starting with the given `index` based on the current strategy.
    async fn restart(&mut self, index: usize) {
        match self.strategy {
            SupervisionStrategy::OneForOne => {
                match self.start_child(index).await {
                    Ok(pid) => {
                        self.children[index].pid = pid;
                    }
                    Err(reason) => {
                        let id = self.children[index].id();

                        #[cfg(feature = "tracing")]
                        tracing::error!(reason = ?reason, child_id = ?id, child_pid = ?self.children[index].pid, "Start error.");

                        Supervisor::cast(
                            Process::current(),
                            SupervisorMessage::TryAgainRestartId(id),
                        );
                    }
                };
            }
            SupervisionStrategy::RestForOne => {
                if let Some((index, reason)) = self.restart_multiple_children(index, false).await {
                    let id = self.children[index].id();

                    #[cfg(feature = "tracing")]
                    tracing::error!(reason = ?reason, child_id = ?id, child_pid = ?self.children[index].pid, "Start error.");

                    Supervisor::cast(Process::current(), SupervisorMessage::TryAgainRestartId(id));
                }
            }
            SupervisionStrategy::OneForAll => {
                if let Some((index, reason)) = self.restart_multiple_children(index, true).await {
                    let id = self.children[index].id();

                    #[cfg(feature = "tracing")]
                    tracing::error!(reason = ?reason, child_id = ?id, child_pid = ?self.children[index].pid, "Start error.");

                    Supervisor::cast(Process::current(), SupervisorMessage::TryAgainRestartId(id));
                }
            }
        }
    }

    /// Restarts multiple children, returning if one of them fails.
    async fn restart_multiple_children(
        &mut self,
        index: usize,
        all: bool,
    ) -> Option<(usize, ExitReason)> {
        let mut indices = Vec::new();

        let range = if all {
            0..self.children.len()
        } else {
            index..self.children.len()
        };

        for tindex in range {
            indices.push(tindex);

            if index == tindex {
                continue;
            }

            self.terminate_child(tindex).await;
        }

        for sindex in indices {
            match self.start_child(sindex).await {
                Ok(pid) => {
                    self.children[sindex].pid = pid;
                }
                Err(reason) => {
                    return Some((sindex, reason));
                }
            }
        }

        None
    }

    /// Tries to restart the given child again, returns if an error occured.
    async fn try_again_restart(&mut self, index: usize) -> Result<(), ExitReason> {
        if self.add_restart() {
            return Err(ExitReason::from("shutdown"));
        }

        self.restart(index).await;

        Ok(())
    }

    /// Starts the given child by it's index and returns what the result was.
    async fn start_child(&mut self, index: usize) -> Result<Option<Pid>, ExitReason> {
        let child = &mut self.children[index];
        let start_child = Pin::from(child.spec.start.as_ref().unwrap()()).await;

        match start_child {
            Ok(pid) => {
                #[cfg(feature = "tracing")]
                tracing::info!(child_id = ?child.spec.id, child_pid = ?pid, "Started child.");

                Ok(Some(pid))
            }
            Err(reason) => {
                if reason.is_ignore() {
                    #[cfg(feature = "tracing")]
                    tracing::info!(child_id = ?child.spec.id, child_pid = ?None::<Pid>, "Started child.");

                    Ok(None)
                } else {
                    Err(reason)
                }
            }
        }
    }

    /// Checks whether or not we should automatically shutdown the supervisor. Returns `true` if so.
    fn check_auto_shutdown(&mut self, child: SupervisedChild) -> bool {
        if matches!(self.auto_shutdown, AutoShutdown::Never) {
            return false;
        }

        if !child.spec.significant {
            return false;
        }

        if matches!(self.auto_shutdown, AutoShutdown::AnySignificant) {
            return true;
        }

        self.children.iter().any(|child| {
            if child.pid.is_none() {
                return false;
            }

            child.spec.significant
        })
    }

    /// Adds another restart to the backlog and returns `true` if we've exceeded our quota of restarts.
    fn add_restart(&mut self) -> bool {
        let now = Instant::now();
        let threshold = now - self.max_duration;

        self.restarts.retain(|restart| *restart >= threshold);
        self.restarts.push(now);

        if self.restarts.len() > self.max_restarts {
            #[cfg(feature = "tracing")]
            tracing::error!(restarts = ?self.restarts.len(), threshold = ?self.max_duration, "Reached max restart intensity.");

            return true;
        }

        false
    }

    /// Counts all of the supervised children.
    fn count_all_children(&mut self) -> SupervisorCounts {
        let mut counts = SupervisorCounts {
            specs: 0,
            active: 0,
            supervisors: 0,
            workers: 0,
        };

        for child in &self.children {
            counts.specs += 1;

            if child.pid.is_some() {
                counts.active += 1;
            }

            if matches!(child.spec.child_type, ChildType::Supervisor) {
                counts.supervisors += 1;
            } else {
                counts.workers += 1;
            }
        }

        counts
    }

    /// Removes a child from the supervisor.
    fn remove_child(&mut self, index: usize) -> SupervisedChild {
        let child = self.children.remove(index);

        self.identifiers.remove(&child.spec.id);

        child
    }

    /// Finds a child by the given `pid`.
    fn find_child(&mut self, pid: Pid) -> Option<usize> {
        self.children
            .iter()
            .position(|child| child.pid.is_some_and(|cpid| cpid == pid))
    }

    /// Finds a child by the given `id`.
    fn find_child_id(&mut self, id: &str) -> Option<usize> {
        self.children.iter().position(|child| child.spec.id == id)
    }
}

impl SupervisedChild {
    /// Returns `true` if the child is a permanent process.
    pub const fn is_permanent(&self) -> bool {
        matches!(self.spec.restart, Restart::Permanent)
    }

    /// Returns `true` if the child is a transient process.
    pub const fn is_transient(&self) -> bool {
        matches!(self.spec.restart, Restart::Transient)
    }

    /// Returns `true` if the child is a temporary process.
    pub const fn is_temporary(&self) -> bool {
        matches!(self.spec.restart, Restart::Temporary)
    }

    /// Returns the unique id of the child.
    pub fn id(&self) -> String {
        self.spec.id.clone()
    }

    /// Returns how the child should be terminated.
    pub const fn shutdown(&self) -> Shutdown {
        match self.spec.shutdown {
            None => match self.spec.child_type {
                ChildType::Worker => Shutdown::Duration(Duration::from_secs(5)),
                ChildType::Supervisor => Shutdown::Infinity,
            },
            Some(shutdown) => shutdown,
        }
    }
}

impl GenServer for Supervisor {
    type InitArg = ();
    type Message = SupervisorMessage;

    async fn init(&mut self, _: Self::InitArg) -> Result<(), ExitReason> {
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        self.init_children().await
    }

    async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
        match message {
            SupervisorMessage::TryAgainRestartPid(pid) => {
                if let Some(index) = self.find_child(pid) {
                    return self.try_again_restart(index).await;
                }
            }
            SupervisorMessage::TryAgainRestartId(id) => {
                if let Some(index) = self.find_child_id(&id) {
                    return self.try_again_restart(index).await;
                }
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        match message {
            SupervisorMessage::CountChildrenRequest => {
                let counts = self.count_all_children();

                Ok(Some(SupervisorMessage::CountChildrenResponse(counts)))
            }
            _ => unreachable!(),
        }
    }

    async fn handle_info(&mut self, info: Message<Self::Message>) -> Result<(), ExitReason> {
        match info {
            Message::System(SystemMessage::Exit(pid, reason)) => {
                self.restart_child(pid, reason).await
            }
            _ => Ok(()),
        }
    }
}

/// Terminates the given `pid` using the given `shutdown` method.
async fn shutdown(pid: Pid, shutdown: Shutdown) -> Result<(), ExitReason> {
    let monitor = Process::monitor(pid);

    match shutdown {
        Shutdown::BrutalKill => shutdown_brutal_kill(pid, monitor).await,
        Shutdown::Duration(timeout) => shutdown_timeout(pid, monitor, timeout).await,
        Shutdown::Infinity => shutdown_infinity(pid, monitor).await,
    }
}

/// Terminates the given `pid` by forcefully killing it and waiting for the `monitor` to fire.
async fn shutdown_brutal_kill(pid: Pid, monitor: Reference) -> Result<(), ExitReason> {
    Process::exit(pid, ExitReason::Kill);

    let result = Process::receiver()
        .ignore_type()
        .select::<(), _>(|message| {
            match message {
                Message::System(SystemMessage::ProcessDown(_, tag, _)) => {
                    // Make sure that the tag matches.
                    *tag == monitor
                }
                _ => false,
            }
        })
        .await;

    let Message::System(SystemMessage::ProcessDown(_, _, reason)) = result else {
        unreachable!()
    };

    unlink_flush(pid, reason);

    Ok(())
}

/// Terminates the given `pid` by gracefully waiting for `timeout`
/// then forcefully kills it as necessary while waiting for `monitor` to fire.
async fn shutdown_timeout(
    pid: Pid,
    monitor: Reference,
    timeout: Duration,
) -> Result<(), ExitReason> {
    Process::exit(pid, ExitReason::from("shutdown"));

    let receiver = Process::receiver()
        .ignore_type()
        .select::<(), _>(|message| {
            match message {
                Message::System(SystemMessage::ProcessDown(_, tag, _)) => {
                    // Make sure that the tag matches.
                    *tag == monitor
                }
                _ => false,
            }
        });

    let result = Process::timeout(timeout, receiver).await;

    match result {
        Ok(Message::System(SystemMessage::ProcessDown(_, _, reason))) => {
            unlink_flush(pid, reason);

            Ok(())
        }
        Ok(_) => unreachable!(),
        Err(_) => shutdown_brutal_kill(pid, monitor).await,
    }
}

/// Terminates the given `pid` by gracefully waiting indefinitely for the `monitor` to fire.
async fn shutdown_infinity(pid: Pid, monitor: Reference) -> Result<(), ExitReason> {
    Process::exit(pid, ExitReason::from("shutdown"));

    let result = Process::receiver()
        .ignore_type()
        .select::<(), _>(|message| {
            match message {
                Message::System(SystemMessage::ProcessDown(_, tag, _)) => {
                    // Make sure that the tag matches.
                    *tag == monitor
                }
                _ => false,
            }
        })
        .await;

    let Message::System(SystemMessage::ProcessDown(_, _, reason)) = result else {
        unreachable!()
    };

    unlink_flush(pid, reason);

    Ok(())
}

/// Unlinks the given process and ensures that any pending exit signal is flushed from the message queue.
///
/// Returns the real [ExitReason] or the `default_reason` if no signal was found.
fn unlink_flush(pid: Pid, default_reason: ExitReason) -> ExitReason {
    Process::unlink(pid);

    let mut reason = default_reason;

    Process::receiver()
        .ignore_type()
        .drop::<(), _>(|message| match message {
            Message::System(SystemMessage::Exit(epid, ereason)) => {
                if *epid == pid {
                    reason = ereason.clone();
                    return true;
                }

                false
            }
            _ => false,
        });

    reason
}
