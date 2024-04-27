use bincode::Decode;
use bincode::Encode;

/// The frame used to install or destroy a process link.
#[derive(Debug, Encode, Decode)]
pub struct Link {
    pub install: bool,
    pub process_id: u64,
    pub from_id: u64,
}

impl Link {
    /// Constructs a new instance of [Link] frame.
    pub const fn new(install: bool, process_id: u64, from_id: u64) -> Self {
        Self {
            install,
            process_id,
            from_id,
        }
    }
}
