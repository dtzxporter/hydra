use std::future::Future;
use std::sync::Arc;

use serde::Deserialize;
use serde::Serialize;

use crate::ExitReason;
use crate::Pid;
use crate::Restart;
use crate::Shutdown;

/// The type of child process.
#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub enum ChildType {
    /// The child is a worker process.
    Worker,
    /// The child is a supervisor process.
    Supervisor,
}

/// The child specification describes how the supervisor starts, shuts down, and restarts child processes.
#[derive(Clone)]
pub struct ChildSpec {
    pub(crate) id: String,
    #[allow(clippy::type_complexity)]
    pub(crate) start: Option<
        Arc<
            dyn Fn() -> Box<dyn Future<Output = Result<Pid, ExitReason>> + Send + Sync>
                + Send
                + Sync,
        >,
    >,
    pub(crate) restart: Restart,
    pub(crate) shutdown: Option<Shutdown>,
    pub(crate) child_type: ChildType,
    pub(crate) significant: bool,
}

impl ChildSpec {
    /// Constructs a new instance of [ChildSpec] with the given id.
    pub fn new<T: Into<String>>(id: T) -> Self {
        Self {
            id: id.into(),
            start: None,
            restart: Restart::Permanent,
            shutdown: None,
            child_type: ChildType::Worker,
            significant: false,
        }
    }

    /// Sets a new id for this [ChildSpec].
    pub fn id<T: Into<String>>(mut self, id: T) -> Self {
        self.id = id.into();
        self
    }

    /// The method invoked to start the child process.
    ///
    /// Must return a future that resolves to [Result<Pid, ExitReason>].
    pub fn start<T, F>(mut self, start: T) -> Self
    where
        T: Fn() -> F + Send + Sync + 'static,
        F: Future<Output = Result<Pid, ExitReason>> + Send + Sync + 'static,
    {
        self.start = Some(Arc::new(move || Box::new(start())));
        self
    }

    /// Defines when a terminated child process should be restarted.
    ///
    /// Defaults to [Restart::Permanent].
    pub const fn restart(mut self, restart: Restart) -> Self {
        self.restart = restart;
        self
    }

    /// Defines how a process should be terminated, specifically how long to wait before forcefully killing it.
    ///
    /// Defaults to 5s if the type is `worker` or infinity if the type is `supervisor`.
    pub fn shutdown<T: Into<Shutdown>>(mut self, shutdown: T) -> Self {
        self.shutdown = Some(shutdown.into());
        self
    }

    /// Specifies the type of child process.
    pub const fn child_type(mut self, child_type: ChildType) -> Self {
        self.child_type = child_type;
        self
    }

    /// A boolean indicating if the child process should be considered significant with regard to automatic shutdown.
    ///
    /// Only `transient` and `temporary` child processes can be marked as significant.
    ///
    /// Defaults to `false`.
    pub const fn significant(mut self, significant: bool) -> Self {
        if significant
            && !matches!(self.restart, Restart::Temporary)
            && !matches!(self.restart, Restart::Transient)
        {
            panic!("Only temporary/transient children can be marked significant!");
        }

        self.significant = significant;
        self
    }
}
