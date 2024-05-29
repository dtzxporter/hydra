use std::time::Duration;

use crate::GenServerOptions;

/// Options used to configure a Supervisor.
#[derive(Debug, Default, Clone)]
pub struct SupervisorOptions {
    pub(crate) name: Option<String>,
    pub(crate) timeout: Option<Duration>,
}

impl SupervisorOptions {
    /// Constructs a new instance of [SupervisorOptions] with the default values.
    pub const fn new() -> Self {
        Self {
            name: None,
            timeout: None,
        }
    }

    /// Specifies a name to register the Supervisor under.
    pub fn name<T: Into<String>>(mut self, name: T) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Specifies a timeout for the Supervisor `init` function.
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }
}

impl From<SupervisorOptions> for GenServerOptions {
    fn from(mut value: SupervisorOptions) -> Self {
        let mut options = GenServerOptions::new();

        if let Some(name) = value.name.take() {
            options = options.name(name);
        }

        if let Some(timeout) = value.timeout.take() {
            options = options.timeout(timeout);
        }

        options
    }
}
