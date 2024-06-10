use serde::Deserialize;
use serde::Serialize;

use tokio::runtime::Handle;

use crate::process_len;

/// Runtime information for the current hydra instance.
#[derive(Serialize, Deserialize)]
pub struct RuntimeInfo {
    /// The crate version of the hydra runtime.
    system_version: String,
    /// The number of logical cpus on the current machine.
    logical_cpus: usize,
    /// The number of worker threads currently in use.
    worker_threads: usize,
    /// The number of processes currently running.
    processes: usize,
}

impl RuntimeInfo {
    /// Constructs and loads [RuntimeInfo] data, must be called from within the runtime.
    pub(super) fn load() -> Self {
        let runtime = Handle::current();

        Self {
            system_version: String::from(env!("CARGO_PKG_VERSION")),
            logical_cpus: std::thread::available_parallelism()
                .map(|cpus| cpus.get())
                .unwrap_or_default(),
            worker_threads: runtime.metrics().num_workers(),
            processes: process_len(),
        }
    }
}
