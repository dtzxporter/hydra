use std::time::Duration;

/// Configuration values for an application.
pub struct ApplicationConfig {
    #[cfg(feature = "tracing")]
    pub(crate) tracing_subscribe: bool,
    #[cfg(feature = "tracing")]
    pub(crate) tracing_panics: bool,
    pub(crate) graceful_shutdown: bool,
    pub(crate) graceful_shutdown_timeout: Duration,
}

impl ApplicationConfig {
    /// Constructs a new instance of [ApplicationConfig].
    pub const fn new() -> Self {
        Self {
            #[cfg(feature = "tracing")]
            tracing_subscribe: false,
            #[cfg(feature = "tracing")]
            tracing_panics: false,
            graceful_shutdown: false,
            graceful_shutdown_timeout: Duration::from_secs(10),
        }
    }

    /// Configure whether or not tracing will subscribe when you call `run`.
    ///
    /// This will install a global tracing subscriber with recommended settings for you.
    #[cfg(feature = "tracing")]
    pub fn with_tracing_subscribe(mut self, value: bool) -> Self {
        self.tracing_subscribe = value;
        self
    }

    /// Configure whether or not tracing will be used to catch panics globally.
    ///
    /// This will install a global panic hook that will log panics using `tracing`.
    #[cfg(feature = "tracing")]
    pub fn with_tracing_panics(mut self, value: bool) -> Self {
        self.tracing_panics = value;
        self
    }

    /// Configure whether or not to listen for shutdown signals and allow the program to cleanup processes before closing.
    pub fn with_graceful_shutdown(mut self, value: bool) -> Self {
        self.graceful_shutdown = value;
        self
    }

    /// Configure the maximum amount of time to wait for the application to shutdown before exiting.
    pub fn with_graceful_shutdown_timeout(mut self, duration: Duration) -> Self {
        self.graceful_shutdown_timeout = duration;
        self
    }
}

impl Default for ApplicationConfig {
    fn default() -> Self {
        Self {
            #[cfg(feature = "tracing")]
            tracing_subscribe: true,
            #[cfg(feature = "tracing")]
            tracing_panics: true,
            graceful_shutdown: true,
            graceful_shutdown_timeout: Duration::from_secs(10),
        }
    }
}
