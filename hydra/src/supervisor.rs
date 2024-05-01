use std::pin::Pin;
use std::time::Duration;

use crate::AutoShutdown;
use crate::ChildSpec;
use crate::ExitReason;
use crate::GenServer;
use crate::GenServerOptions;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;

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

    async fn start_children(&mut self) -> Result<(), ExitReason> {
        for child in self.children.iter_mut() {
            let start_child = Pin::from(child.spec.start.as_ref().unwrap()()).await;

            match start_child {
                Ok(pid) => child.pid = Some(pid),
                Err(reason) => {
                    if reason.is_ignore() {
                        continue;
                    }

                    return Err(reason);
                }
            }
        }

        Ok(())
    }

    async fn terminate_children(&mut self) -> Result<(), ExitReason> {
        for child in self.children.iter_mut().rev() {
            let Some(pid) = child.pid.take() else {
                continue;
            };

            Process::exit(pid, ExitReason::from("shutdown"));
        }

        Ok(())
    }

    async fn init_children(&mut self) -> Result<(), ExitReason> {
        if let Err(reason) = self.start_children().await {
            let _ = self.terminate_children().await;

            return Err(reason);
        }

        Ok(())
    }
}

impl GenServer for Supervisor {
    type InitArg = ();
    type Message = ();

    async fn init(&mut self, _: Self::InitArg) -> Result<(), ExitReason> {
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        // A static supervisor just needs to initialize the children.
        self.init_children().await
    }
}
