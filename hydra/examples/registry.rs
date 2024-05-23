use std::time::Duration;

use hydra::ProcessFlags;
use hydra::Shutdown;
use serde::Deserialize;
use serde::Serialize;

use hydra::Application;
use hydra::ChildSpec;
use hydra::ExitReason;
use hydra::GenServer;
use hydra::GenServerOptions;
use hydra::Pid;
use hydra::Process;
use hydra::Registry;
use hydra::RegistryKey;
use hydra::SupervisionStrategy;
use hydra::Supervisor;

async fn test_registry() {
    Registry::start("space-registry", "my awesome space id")
        .await
        .expect("Failed to start process");

    Registry::start("space-registry", "my lame space id")
        .await
        .expect("Failed to start process");

    let pid = Registry::lookup("space-registry", "my awesome space id")
        .await
        .expect("Failed to lookup process");

    tracing::info!("Looked up space process: {:?}", pid);

    let count = Registry::count("space-registry")
        .await
        .expect("Failed to count processes");

    tracing::info!("Count of registered processes: {:?}", count);

    let list = Registry::list("space-registry")
        .await
        .expect("Failed to list processes");

    tracing::info!("List of registered processes: {:?}", list);
}

#[derive(Debug, Serialize, Deserialize)]
enum MyMessage {
    // No messages used.
}

struct MyApplication;

impl Application for MyApplication {
    const GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(20);

    async fn start(&self) -> Result<Pid, ExitReason> {
        // Spawn a registry that will take care of registering 'MySpace'.
        let children = [
            Registry::new("space-registry")
                .with_start(|key| {
                    let RegistryKey::String(id) = key else {
                        panic!()
                    };

                    MySpace::new(id).start_link(GenServerOptions::new())
                })
                .with_shutdown(Shutdown::Infinity)
                .child_spec(GenServerOptions::new())
                .id("space-registry"),
            ChildSpec::new("test-registry")
                .start(move || async { Ok(Process::spawn(test_registry())) }),
        ];

        // Restart only the terminated child.
        Supervisor::with_children(children)
            .strategy(SupervisionStrategy::OneForOne)
            .start_link(GenServerOptions::new())
            .await
    }
}

#[derive(Clone)]
struct MySpace {
    id: String,
}

impl MySpace {
    /// Constructs a new [MySpace].
    pub fn new(id: String) -> Self {
        Self { id }
    }
}

impl GenServer for MySpace {
    type Message = MyMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        tracing::info!("Init MySpace for {:?}", self.id);

        Ok(())
    }

    async fn terminate(&mut self, _reason: ExitReason) {
        tracing::info!("Shutting down MySpace! {:?}", self.id);

        Process::sleep(Duration::from_secs(5)).await;
    }
}

fn main() {
    // This method will only return once the supervisor linked in `start` has terminated.
    Application::run(MyApplication)
}
