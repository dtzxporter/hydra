use std::time::Duration;

/// Options used to configure a [GenServer].
#[derive(Debug, Default, Clone)]
pub struct GenServerOptions {
    pub(crate) name: Option<String>,
    pub(crate) timeout: Option<Duration>,
}

impl GenServerOptions {
    /// Constructs a new instance of [GenServerOptions] with the default values.
    pub const fn new() -> Self {
        Self {
            name: None,
            timeout: None,
        }
    }

    /// Specifies a name to register the [GenServer] under.
    pub fn name<T: Into<String>>(mut self, name: T) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Specifies a timeout for the [GenServer] `init` function.
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }
}
