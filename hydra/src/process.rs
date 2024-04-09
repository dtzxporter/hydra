use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use serde::de::DeserializeOwned;

use flume::Receiver;

use crate::CatchUnwind;
use crate::Dest;
use crate::ExitReason;
use crate::Message;
use crate::MessageState;
use crate::Monitor;
use crate::Pid;
use crate::ProcessFlags;
use crate::ProcessReceiver;
use crate::ProcessRegistration;
use crate::SystemMessage;
use crate::PID_ID_MAXIMUM;
use crate::PROCESS_REGISTRY;

/// A light weight task that can send and receive messages.
pub struct Process {
    /// The unique id of this process.
    pub(crate) pid: Pid,
    /// The inbox for this process.
    pub(crate) channel: Receiver<MessageState>,
}

tokio::task_local! {
    /// Current process information.
    pub(crate) static PROCESS: Process;
}

impl Process {
    /// Constructs a new [Process] from the given [Pid] and channel.
    pub(crate) const fn new(pid: Pid, channel: Receiver<MessageState>) -> Self {
        Self { pid, channel }
    }

    /// Returns the current [Pid].
    #[must_use]
    pub fn current() -> Pid {
        PROCESS.with(|process| process.pid)
    }

    /// Returns the [Pid] registered under `name` or [None] if the name is not registered.
    #[must_use]
    pub fn whereis<S: AsRef<str>>(name: S) -> Option<Pid> {
        PROCESS_REGISTRY
            .read()
            .unwrap()
            .named_processes
            .get(name.as_ref())
            .map(|process_id| Pid::local(*process_id))
    }

    /// Sends a single message to `dest` with the given `message`.
    pub fn send<D: Into<Dest>, M: Send + 'static>(dest: D, message: M) {
        let dest = dest.into();

        match dest {
            Dest::Pid(pid) => {
                if pid.is_local() {
                    PROCESS_REGISTRY
                        .read()
                        .unwrap()
                        .processes
                        .get(&pid.id())
                        .map(|process| process.channel.send(Message::User(message).into()));
                }
            }
            Dest::Named(name) => {
                let registry = PROCESS_REGISTRY.read().unwrap();

                registry
                    .named_processes
                    .get(name.as_ref())
                    .and_then(|id| registry.processes.get(id))
                    .map(|process| process.channel.send(Message::User(message).into()));
            }
            Dest::RemoteNamed(_, _) => {
                unimplemented!("Send remote named not supported!")
            }
        }
    }

    /// Receives a single message that matches the given type from the current processes mailbox or panics.
    #[must_use]
    pub async fn receive<T: DeserializeOwned + Send + 'static>() -> Message<T> {
        PROCESS
            .with(|process| process.channel.clone())
            .recv_async()
            .await
            .unwrap()
            .try_into()
            .unwrap()
    }

    /// Receives a single filtered message that matches the given type from the current processes mailbox.
    #[must_use]
    pub async fn filter_receive<T: DeserializeOwned + Send + 'static>(&self) -> Message<T> {
        Self::receiver().filter_receive().await
    }

    /// Creates a peakable view of the current processes mailbox which allows skipping values we don't want while maintaining order.
    ///
    /// By default, `receive()` will remove the message from the processes mailbox, removing it's unique order.
    /// When using `receiver()` you can call `keep(msg)` to keep the message at it's current offset in the mailbox.
    ///
    /// This was done because it's less efficient to support keeping the message order.
    #[must_use]
    pub fn receiver() -> ProcessReceiver {
        let current = Self::current();
        let receiver = PROCESS.with(|process| process.channel.clone());

        let mut registry = PROCESS_REGISTRY.write().unwrap();
        let process = registry.processes.get_mut(&current.id()).unwrap();

        let receiver = ProcessReceiver::new(current, process.channel.clone(), receiver);

        process.channel = receiver.sender();

        receiver
    }

    /// Spawns the given `function` as a process and returns it's [Pid].
    pub fn spawn<T>(function: T) -> Pid
    where
        T: Future<Output = ()> + Send + 'static,
        T::Output: Send + 'static,
    {
        match spawn_internal(function, false, false) {
            SpawnResult::Pid(pid) => pid,
            SpawnResult::PidMonitor(_, _) => unreachable!(),
        }
    }

    /// Spawns the given `function` as a process, creates a link between the calling process, and returns the new [Pid].
    pub fn spawn_link<T>(function: T) -> Pid
    where
        T: Future<Output = ()> + Send + 'static,
        T::Output: Send + 'static,
    {
        match spawn_internal(function, true, false) {
            SpawnResult::Pid(pid) => pid,
            SpawnResult::PidMonitor(_, _) => unreachable!(),
        }
    }

    /// Spawns the given `function` as a process, creates a monitor for the calling process, and returns the new [Pid].
    pub fn spawn_monitor<T>(function: T) -> (Pid, Monitor)
    where
        T: Future<Output = ()> + Send + 'static,
        T::Output: Send + 'static,
    {
        match spawn_internal(function, false, true) {
            SpawnResult::Pid(_) => unreachable!(),
            SpawnResult::PidMonitor(pid, monitor) => (pid, monitor),
        }
    }

    /// Returns true if the given [Pid] is alive on the local node.
    #[must_use]
    pub fn alive<T: Into<Pid>>(pid: T) -> bool {
        let pid = pid.into();

        if !pid.is_local() {
            panic!("Expected a local pid!");
        }

        PROCESS_REGISTRY
            .read()
            .unwrap()
            .processes
            .contains_key(&pid.id())
    }

    /// Registers the given [Pid] under `name` if the process is local, active, and the name is not already registered.
    pub fn register<T: Into<Pid>, S: Into<String>>(pid: T, name: S) {
        let pid = pid.into();
        let name = name.into();

        if !pid.is_local() {
            panic!("Expected local pid for register!");
        }

        let mut registry = PROCESS_REGISTRY.write().unwrap();

        if registry.named_processes.contains_key(&name) {
            panic!("Name {:?} registered to another process!", name);
        }

        let process = registry
            .processes
            .get(&pid.id())
            .expect("Process does not exist!");

        if process.name.is_some() {
            panic!("Process {:?} was already registered!", pid);
        }

        if let Some(process) = registry.processes.get_mut(&pid.id()) {
            process.name = Some(name.clone());
        }

        registry.named_processes.insert(name, pid.id());
    }

    /// Removes the registered `name`, associated with a [Pid].
    pub fn unregister<S: Into<String>>(name: S) {
        let name = name.into();

        let mut registry = PROCESS_REGISTRY.write().unwrap();

        if let Some(named_pid) = registry.named_processes.remove(&name) {
            if let Some(process) = registry.processes.get_mut(&named_pid) {
                process.name = None;
            }
        } else {
            panic!("Name {:?} was not registered!", name);
        }
    }

    /// Returns a [Vec] of registered process names.
    #[must_use]
    pub fn registered() -> Vec<String> {
        PROCESS_REGISTRY
            .read()
            .unwrap()
            .named_processes
            .keys()
            .cloned()
            .collect()
    }

    /// Returns a [Vec] of [Pid]'s on the local node.
    #[must_use]
    pub fn list() -> Vec<Pid> {
        PROCESS_REGISTRY
            .read()
            .unwrap()
            .processes
            .keys()
            .copied()
            .map(Pid::local)
            .collect()
    }

    /// Creates a bi-directional link between the current process and the given process.
    pub fn link<T: Into<Pid>>(pid: T) {
        let current = Self::current();
        let pid = pid.into();

        if pid == current {
            return;
        }

        if !pid.is_local() {
            unimplemented!("Remote process link unsupported!");
        }

        let mut registry = PROCESS_REGISTRY.write().unwrap();

        if let Some(process) = registry.processes.get_mut(&pid.id()) {
            process.links.insert(current);

            registry
                .processes
                .get_mut(&current.id())
                .map(|x| x.links.insert(pid));
        } else {
            panic!("Process was not an existing process!")
        }
    }

    /// Removes the link between the calling process and the given process.
    pub fn unlink<T: Into<Pid>>(pid: T) {
        let current = Self::current();
        let pid = pid.into();

        if pid == current {
            return;
        }

        if !pid.is_local() {
            unimplemented!("Remote process link unsupported!");
        }

        let mut registry = PROCESS_REGISTRY.write().unwrap();

        registry
            .processes
            .get_mut(&current.id())
            .map(|x| x.links.remove(&pid));

        registry
            .processes
            .get_mut(&pid.id())
            .map(|x| x.links.remove(&current));
    }

    /// Starts monitoring the given [Pid] from the calling process. If the [Pid] is already dead a message is sent immediately.
    pub fn monitor<T: Into<Dest>>(dest: T) -> Monitor {
        let current = Self::current();
        let dest = dest.into();

        let Dest::Pid(pid) = dest else {
            unimplemented!("Dest monitor not supported!")
        };

        if pid == current {
            panic!("Can not monitor yourself!");
        }

        if !pid.is_local() {
            unimplemented!("Remote process monitor unsupported!");
        }

        let mut registry = PROCESS_REGISTRY.write().unwrap();

        let next_monitor_id = registry
            .processes
            .get_mut(&current.id())
            .map(|process| process.next_monitor())
            .unwrap();

        if let Some(process) = registry.processes.get_mut(&pid.id()) {
            process.monitors.insert((current, next_monitor_id));

            registry
                .processes
                .get_mut(&current.id())
                .map(|process| process.installed_monitors.insert(next_monitor_id, pid));
        } else {
            registry
                .processes
                .get(&current.id())
                .map(|process| process.channel.clone())
                .unwrap()
                .send(MessageState::System(SystemMessage::ProcessDown(
                    pid,
                    Monitor::new(current, next_monitor_id),
                    ExitReason::Custom(String::from("noproc")),
                )))
                .unwrap();
        }

        Monitor::new(current, next_monitor_id)
    }

    /// Demonitors the monitor identifier by the given [Monitor] reference.
    pub fn demonitor(monitor: Monitor) {
        if !monitor.pid().is_local() {
            unimplemented!("Remote process monitor unsupported!");
        }

        let mut registry = PROCESS_REGISTRY.write().unwrap();

        if let Some(process) = registry.processes.get_mut(&monitor.pid().id()) {
            process.installed_monitors.remove(&monitor.reference());
        }
    }

    /// Returns the current process flags.
    #[must_use]
    pub fn flags() -> ProcessFlags {
        PROCESS_REGISTRY
            .read()
            .unwrap()
            .processes
            .get(&Self::current().id())
            .unwrap()
            .flags()
    }

    /// Sets one or more process flags.
    pub fn set_flags(flags: ProcessFlags) {
        PROCESS_REGISTRY
            .read()
            .unwrap()
            .processes
            .get(&Self::current().id())
            .unwrap()
            .set_flags(flags)
    }
}

impl Drop for Process {
    fn drop(&mut self) {
        let mut registry = PROCESS_REGISTRY.write().unwrap();

        let process = registry.processes.remove(&self.pid.id()).unwrap();

        if let Some(name) = process.name {
            registry.named_processes.remove(&name);
        }

        for link in process.links {
            if !link.is_local() {
                unimplemented!("Remote process link unsupported!");
            }

            registry.exit_signal_linked_process(link, self.pid, process.exit_reason.clone());
        }

        for (monitor, monitor_id) in process.monitors {
            if !monitor.is_local() {
                unimplemented!("Remote process monitor unsupported!");
            }

            registry.exit_signal_monitored_process(
                monitor,
                monitor_id,
                self.pid,
                process.exit_reason.clone(),
            );
        }

        for (monitor_id, monitor) in process.installed_monitors {
            if !monitor.is_local() {
                unimplemented!("Remote process monitor unsupported!");
            }

            registry.uninstall_monitored_process(monitor, monitor_id, self.pid);
        }
    }
}

/// Internal spawn result.
enum SpawnResult {
    Pid(Pid),
    PidMonitor(Pid, Monitor),
}

/// Internal spawn utility.
fn spawn_internal<T>(function: T, link: bool, monitor: bool) -> SpawnResult
where
    T: Future<Output = ()> + Send + 'static,
    T::Output: Send + 'static,
{
    static ID: AtomicU64 = AtomicU64::new(1);

    let mut registry = PROCESS_REGISTRY.write().unwrap();
    let mut next_id = ID.fetch_add(1, Ordering::Relaxed);

    // Guard against spawning more processes than we can allocate ids for.
    if registry.processes.len() >= PID_ID_MAXIMUM as usize {
        panic!("Maximum number of processes spawned!");
    }

    loop {
        // Zero is a reserved system process id.
        if next_id == 0 {
            next_id = ID.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        // Make sure the id is lower than the maximum id value, and not already taken.
        if next_id > PID_ID_MAXIMUM || registry.processes.contains_key(&next_id) {
            next_id = ID.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        // We found an unused id.
        break;
    }

    let (tx, rx) = flume::unbounded();

    let pid = Pid::local(next_id);
    let process = Process::new(pid, rx);

    // Spawn the process with the newly created process object in scope.
    let handle = tokio::spawn(PROCESS.scope(process, async move {
        if let Err(e) = CatchUnwind::new(AssertUnwindSafe(function)).await {
            if let Some(process) = PROCESS_REGISTRY
                .write()
                .unwrap()
                .processes
                .get_mut(&Process::current().id())
            {
                process.exit_reason = e.into();
            }
        }
    }));

    let mut registration = ProcessRegistration::new(handle, tx);

    // If a link was requested insert it before closing the lock.
    if link {
        let current = Process::current();

        registration.links.insert(current);

        registry
            .processes
            .get_mut(&current.id())
            .map(|process| process.links.insert(pid));
    }

    let mut result = SpawnResult::Pid(pid);

    // If a monitor was requested insert it before closing the lock.
    if monitor {
        let current = Process::current();
        let next_monitor_id = registration.next_monitor();

        registration.monitors.insert((current, next_monitor_id));

        registry
            .processes
            .get_mut(&current.id())
            .map(|process| process.installed_monitors.insert(next_monitor_id, pid));

        result = SpawnResult::PidMonitor(pid, Monitor::new(current, next_monitor_id));
    }

    registry.processes.insert(next_id, registration);

    result
}
