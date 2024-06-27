use serde::Deserialize;
use serde::Serialize;

use tokio::runtime::Handle;

use crate::process_len;

/// Runtime information for the current hydra instance.
#[derive(Serialize, Deserialize)]
pub struct RuntimeInfo {
    /// The crate version of the hydra runtime.
    pub system_version: String,
    /// The number of logical cpus on the current machine.
    pub logical_cpus: usize,
    /// The number of worker threads currently in use.
    pub worker_threads: usize,
    /// The number of processes currently running.
    pub processes: usize,
    /// The "physical" memory used by this process, in bytes.
    /// This corresponds to the following
    /// metric on each platform:
    /// - **Linux, Android, MacOS, iOS**: Resident Set Size.
    /// - **Windows**: Working Set.
    pub physical_memory: usize,
    /// The "virtual" memory used by this process, in bytes.
    /// This corresponds to the following
    /// metric on each platform:
    /// - **Linux, Android, MacOS, iOS**: Virtual Size.
    /// - **Windows**: Pagefile Usage.
    pub virtual_memory: usize,
}

impl RuntimeInfo {
    /// Constructs and loads [RuntimeInfo] data, must be called from within the runtime.
    pub(super) fn load() -> Self {
        let runtime = Handle::current();

        let (physical_memory, virtual_memory) = memory_stats::memory_stats()
            .map(|stats| (stats.physical_mem, stats.virtual_mem))
            .unwrap_or_default();

        Self {
            system_version: String::from(env!("CARGO_PKG_VERSION")),
            logical_cpus: std::thread::available_parallelism()
                .map(|cpus| cpus.get())
                .unwrap_or_default(),
            worker_threads: runtime.metrics().num_workers(),
            processes: process_len(),
            physical_memory,
            virtual_memory,
        }
    }
}
