use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use flume::Receiver;

use crate::CatchUnwind;
use crate::Message;
use crate::MessageState;
use crate::Pid;
use crate::ProcessFlags;
use crate::ProcessReceiver;
use crate::ProcessRegistration;
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
    pub fn send<M: Send + 'static>(dest: Pid, message: M) {
        if dest.is_local() {
            PROCESS_REGISTRY
                .read()
                .unwrap()
                .processes
                .get(&dest.id())
                .map(|process| process.channel.send(Message::User(message).into()));
        }
    }

    /// Receives a single message from the current processes mailbox.
    #[must_use]
    pub async fn receive<T: Send + 'static>() -> Message<T> {
        PROCESS
            .with(|process| process.channel.clone())
            .recv_async()
            .await
            .unwrap()
            .into()
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
        spawn_internal(function, false, false)
    }

    /// Spawns the given `function` as a process, creates a link between the calling process, and returns the new [Pid].
    pub fn spawn_link<T>(function: T) -> Pid
    where
        T: Future<Output = ()> + Send + 'static,
        T::Output: Send + 'static,
    {
        spawn_internal(function, true, false)
    }

    /// Spawns the given `function` as a process, creates a monitor for the calling process, and returns the new [Pid].
    pub fn spawn_monitor<T>(function: T) -> Pid
    where
        T: Future<Output = ()> + Send + 'static,
        T::Output: Send + 'static,
    {
        spawn_internal(function, false, true)
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
    }
}

/// Internal spawn utility.
fn spawn_internal<T>(function: T, link: bool, monitor: bool) -> Pid
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

    // If a monitor was requested insert it before closing the lock.
    if monitor {
        unimplemented!()
    }

    registry.processes.insert(next_id, registration);

    pid
}
