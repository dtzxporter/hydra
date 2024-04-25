use bincode::Decode;
use bincode::Encode;

/// The frame used to install or destroy a process monitor.
#[derive(Debug, Encode, Decode)]
pub struct Monitor {
    pub install: bool,
    pub process_id: u64,
    pub from_id: Option<u64>,
    pub reference_id: u64,
}

impl Monitor {
    /// Constructs a new instance of [Monitor] frame.
    pub const fn new(
        install: bool,
        process_id: u64,
        from_id: Option<u64>,
        reference_id: u64,
    ) -> Self {
        Self {
            install,
            process_id,
            from_id,
            reference_id,
        }
    }
}
