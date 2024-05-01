use std::time::Duration;

/// Defines how a child process should be terminated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Shutdown {
    /// The child process is unconditionally and immediately terminated using `Process::exit(child, ExitReason::Kill)`.
    BrutalKill,
    /// The amount of time that the supervisor will wait for it's children to terminate after emitting a
    /// `Process::exit(child, ExitReason::from("shutdown"))` signal. If the child process is not trapping exits, the initial `shutdown` signal
    /// will terminate the child process immediately. If the child process is trapping exits, it has the given duration to terminate.
    Duration(Duration),
    /// The supervisor will wait indefinitely for the child to terminate.
    Infinity,
}

/// Defines how a superviser should handle shutdown when a significant process exits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AutoShutdown {
    /// This is the default, automatic shutdown is disabled.
    Never,
    /// If any significant child process exits, the supervisor will automatically shut down it's children, then itself.
    AnySignificant,
    /// When all significant child processes have exited, the supervisor will automatically shut down it's children, then itself.
    AllSignificant,
}

impl From<Duration> for Shutdown {
    fn from(value: Duration) -> Self {
        Self::Duration(value)
    }
}
