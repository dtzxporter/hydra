use bincode::Decode;
use bincode::Encode;

/// The frame used to update an installed process monitor.
#[derive(Debug, Encode, Decode)]
pub struct MonitorUpdate {
    pub process_id: u64,
    pub from_id: u64,
    pub reference_id: u64,
}

impl MonitorUpdate {
    /// Constructs a new instance of [MonitorUpdate] frame.
    pub const fn new(process_id: u64, from_id: u64, reference_id: u64) -> Self {
        Self {
            process_id,
            from_id,
            reference_id,
        }
    }
}
