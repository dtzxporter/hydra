use crate::GenServerOptions;

/// Options used to configure a Registry.
#[derive(Debug, Default, Clone)]
pub struct RegistryOptions {
    pub(crate) name: Option<String>,
}

impl RegistryOptions {
    /// Constructs a new instance of [RegistryOptions] with the default values.
    pub const fn new() -> Self {
        Self { name: None }
    }

    /// Specifies a name to register the Registry under.
    pub fn name<T: Into<String>>(mut self, name: T) -> Self {
        self.name = Some(name.into());
        self
    }
}

impl From<RegistryOptions> for GenServerOptions {
    fn from(mut value: RegistryOptions) -> Self {
        let mut options = GenServerOptions::new();

        if let Some(name) = value.name.take() {
            options = options.name(name);
        }

        options
    }
}
