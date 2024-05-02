use std::collections::BTreeSet;
use std::collections::VecDeque;
use std::pin::Pin;
use std::time::Duration;
use std::time::Instant;

use crate::AutoShutdown;
use crate::ChildSpec;
use crate::ChildType;
use crate::ExitReason;
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
    restarts: VecDeque<Instant>,
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
            restarts: VecDeque::new(),
            strategy: SupervisionStrategy::OneForOne,
            auto_shutdown: AutoShutdown::Never,
            max_restarts: 3,
            max_duration: Duration::from_secs(5),
        }
    }

    /// Constructs a new instance of [Supervisor] with the given children.
    pub fn with_children<T: IntoIterator<Item = ChildSpec>>(children: T) -> Self {
        Self {
            children: Vec::from_iter(
                children
                    .into_iter()
                    .map(|spec| SupervisedChild { spec, pid: None }),
            ),
            ..Self::new()
        }
    }

    /// Adds a child to this [Supervisor].
    pub fn add_child(mut self, child: ChildSpec) -> Self {
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

    pub async fn start_link(self, options: GenServerOptions) -> Result<Pid, ExitReason> {
        GenServer::start_link(self, (), options).await
    }

    /// Starts all of the children.
    async fn start_children(&mut self) -> Result<(), ExitReason> {
        let mut remove: Vec<usize> = Vec::new();

        for (index, child) in self.children.iter_mut().enumerate() {
            let start_child = Pin::from(child.spec.start.as_ref().unwrap()()).await;

            match start_child {
                Ok(pid) => {
                    child.pid = Some(pid);
                }
                Err(reason) => {
                    if reason.is_ignore() {
                        if child.is_temporary() {
                            remove.push(index);
                        }
                        continue;
                    }

                    return Err(ExitReason::from("failed_to_start_child"));
                }
            }
        }

        for index in remove.into_iter().rev() {
            self.children.remove(index);
        }

        Ok(())
    }

    /// Terminates all of the children.
    async fn terminate_children(&mut self) -> Result<(), ExitReason> {
        let mut remove: Vec<usize> = Vec::new();

        for (index, child) in self.children.iter_mut().enumerate().rev() {
            if child.is_temporary() {
                remove.push(index);
            }

            let Some(pid) = child.pid.take() else {
                continue;
            };

            if let Err(reason) = shutdown(pid, child.shutdown()).await {
                println!("Report error: {:?}", reason);
            }
        }

        for index in remove {
            self.children.remove(index);
        }

        Ok(())
    }

    /// Checks all of the children for correct specification and then starts them.
    async fn init_children(&mut self) -> Result<(), ExitReason> {
        self.check_child_specs();

        if let Err(reason) = self.start_children().await {
            let _ = self.terminate_children().await;

            return Err(reason);
        }

        Ok(())
    }

    async fn restart_child(&mut self, pid: Pid, reason: ExitReason) -> Result<(), ExitReason> {
        let Some(index) = self.find_child(pid) else {
            return Ok(());
        };

        // https://github.com/erlang/otp/blob/master/lib/stdlib/src/supervisor.erl#L1157
        let child = &mut self.children[index];

        // This child has died, do something!

        Ok(())
    }

    fn add_restart(&mut self) {
        // https://github.com/erlang/otp/blob/master/lib/stdlib/src/supervisor.erl#L2075
        let now = Instant::now();
    }

    fn check_child_specs(&mut self) {
        let mut ids: BTreeSet<&str> = BTreeSet::new();

        for child in &self.children {
            if !ids.insert(&child.spec.id) {
                panic!("Found non-unique child spec id: {:?}!", child.spec.id);
            }
        }
    }

    fn find_child(&mut self, pid: Pid) -> Option<usize> {
        self.children
            .iter()
            .position(|child| child.pid.is_some_and(|cpid| cpid == pid))
    }
}

impl SupervisedChild {
    /// Returns `true` if the child is a temporary process.
    pub const fn is_temporary(&self) -> bool {
        matches!(self.spec.restart, Restart::Temporary)
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
    type Message = ();

    async fn init(&mut self, _: Self::InitArg) -> Result<(), ExitReason> {
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        self.init_children().await
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

async fn shutdown(pid: Pid, shutdown: Shutdown) -> Result<(), ExitReason> {
    let monitor = Process::monitor(pid);

    match shutdown {
        Shutdown::BrutalKill => shutdown_brutal_kill(pid, monitor).await,
        Shutdown::Duration(timeout) => shutdown_timeout(pid, monitor, timeout).await,
        Shutdown::Infinity => shutdown_infinity(pid, monitor).await,
    }
}

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
