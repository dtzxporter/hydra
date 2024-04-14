use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use flume::Receiver;
use flume::Sender;

use crate::alias_create;
use crate::alias_destroy;
use crate::alias_destroy_all;
use crate::alias_retrieve;
use crate::monitor_create;
use crate::monitor_destroy;
use crate::monitor_destroy_all;
use crate::monitor_process_down;
use crate::CatchUnwind;
use crate::Dest;
use crate::ExitReason;
use crate::Message;
use crate::Pid;
use crate::ProcessFlags;
use crate::ProcessItem;
use crate::ProcessReceiver;
use crate::ProcessRegistration;
use crate::Receivable;
use crate::Reference;
use crate::SystemMessage;
use crate::PROCESS_REGISTRY;

/// The send type for a process.
pub(crate) type ProcessSend = Sender<ProcessItem>;
/// The receive type for a process.
pub(crate) type ProcessReceive = Receiver<ProcessItem>;

/// A light weight task that can send and receive messages.
pub struct Process {
    /// The unique id of this process.
    pub(crate) pid: Pid,
    /// The sender for this process.
    pub(crate) sender: ProcessSend,
    /// The receiver for this process.
    pub(crate) receiver: ProcessReceive,
    /// The messages already popped from the receiver for this process.
    pub(crate) items: RefCell<Vec<ProcessItem>>,
    /// A collection of process aliases.
    pub(crate) aliases: RefCell<BTreeSet<u64>>,
    /// A collection of process monitor references.
    pub(crate) monitors: RefCell<BTreeMap<Reference, Pid>>,
}

tokio::task_local! {
    /// Current process information.
    pub(crate) static PROCESS: Process;
}

impl Process {
    /// Constructs a new [Process] from the given [Pid] and channel.
    pub(crate) fn new(pid: Pid, sender: ProcessSend, receiver: ProcessReceive) -> Self {
        Self {
            pid,
            sender,
            receiver,
            items: RefCell::new(Vec::new()),
            aliases: RefCell::new(BTreeSet::new()),
            monitors: RefCell::new(BTreeMap::new()),
        }
    }

    /// Creates a process alias. If reply is `true` the alias deactivates when the first message is received.
    ///
    /// Otherwise, you need to call `unalias(reference)` to deactivate the alias.
    pub fn alias(reply: bool) -> Reference {
        let reference = Reference::new();

        let sender = PROCESS.with(|process| {
            process.aliases.borrow_mut().insert(reference.id());
            process.sender.clone()
        });

        alias_create(sender, reference, reply);

        reference
    }

    /// Explicitly deactivates a process alias.
    ///
    /// Returns `true` if the alias was currently-active for the current process, or `false` otherwise.
    pub fn unalias(alias: Reference) -> bool {
        PROCESS.with(|process| process.aliases.borrow_mut().remove(&alias.id()));

        alias_destroy(alias)
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
    pub fn send<D: Into<Dest>, M: Receivable>(dest: D, message: M) {
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
                } else {
                    unimplemented!("Send remote pid not supported!")
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
            Dest::Alias(reference) => {
                if reference.is_local() {
                    alias_retrieve(reference)
                        .map(|alias| alias.sender.send(Message::User(message).into()));
                } else {
                    unimplemented!("Send alias not supported!")
                }
            }
        }
    }

    /// Creates a receiver with optional filtering for a single message from the current processes mailbox.
    #[must_use]
    pub fn receiver() -> ProcessReceiver {
        ProcessReceiver::new()
    }

    /// Creates a receiver for a single message that matches the given type from the current processes mailbox.
    ///
    /// This will panic if a message is received that doesn't match the given type.
    #[must_use]
    pub async fn receive<T: Receivable>() -> Message<T> {
        ProcessReceiver::new().receive().await
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
    pub fn spawn_monitor<T>(function: T) -> (Pid, Reference)
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
    pub fn alive(pid: Pid) -> bool {
        if pid.is_remote() {
            panic!("Expected a local pid!");
        }

        PROCESS_REGISTRY
            .read()
            .unwrap()
            .processes
            .contains_key(&pid.id())
    }

    /// Registers the given [Pid] under `name` if the process is local, active, and the name is not already registered.
    pub fn register<S: Into<String>>(pid: Pid, name: S) {
        let name = name.into();

        if pid.is_remote() {
            panic!("Expected local pid for register!");
        }

        let mut registry = PROCESS_REGISTRY.write().unwrap();

        if registry.named_processes.contains_key(&name) {
            drop(registry);
            panic!("Name {:?} registered to another process!", name);
        }

        let process = registry
            .processes
            .get(&pid.id())
            .expect("Process does not exist!");

        if process.name.is_some() {
            drop(registry);
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
            drop(registry);
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
    pub fn link(pid: Pid) {
        let current = Self::current();

        if pid == current {
            return;
        }

        if pid.is_remote() {
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
            drop(registry);
            panic!("Process was not an existing process!")
        }
    }

    /// Removes the link between the calling process and the given process.
    pub fn unlink(pid: Pid) {
        let current = Self::current();

        if pid == current {
            return;
        }

        if pid.is_remote() {
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
    pub fn monitor<T: Into<Dest>>(dest: T) -> Reference {
        let current = Self::current();
        let dest = dest.into();

        let Dest::Pid(pid) = dest else {
            unimplemented!("Dest monitor not supported!")
        };

        if pid == current {
            panic!("Can not monitor yourself!");
        }

        if pid.is_remote() {
            unimplemented!("Remote process monitor unsupported!");
        }

        let reference = Reference::new();

        PROCESS.with(|process| process.monitors.borrow_mut().insert(reference, pid));

        let registry = PROCESS_REGISTRY.read().unwrap();

        if registry.processes.contains_key(&pid.id()) {
            monitor_create(pid, reference, current);
        } else {
            PROCESS.with(|process| {
                process
                    .sender
                    .send(ProcessItem::SystemMessage(SystemMessage::ProcessDown(
                        pid,
                        reference,
                        "noproc".into(),
                    )))
                    .unwrap()
            });
        }

        reference
    }

    /// Demonitors the monitor identifier by the given reference.
    ///
    /// If a monitor message was sent to the process already but was not received, it will be discarded automatically.
    pub fn demonitor(monitor: Reference) {
        let Some(pid) = PROCESS.with(|process| process.monitors.borrow_mut().remove(&monitor))
        else {
            return;
        };

        if pid.is_local() {
            monitor_destroy(pid, monitor);
        } else {
            unimplemented!("Remote process monitor unsupported!");
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

    /// Sends an exit signal with the given reason to [Pid].
    pub fn exit<E: Into<ExitReason>>(pid: Pid, exit_reason: E) {
        let exit_reason = exit_reason.into();

        if pid.is_remote() {
            unimplemented!("Remote process exit unsupported!");
        }

        PROCESS_REGISTRY
            .write()
            .unwrap()
            .exit_process(pid, Self::current(), exit_reason);
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
            if link.is_remote() {
                unimplemented!("Remote process link unsupported!");
            }

            registry.exit_signal_linked_process(link, self.pid, process.exit_reason.clone());
        }

        drop(registry);

        monitor_process_down(self.pid, process.exit_reason);
        monitor_destroy_all(self.monitors.borrow().iter());

        alias_destroy_all(self.aliases.borrow().iter());
    }
}

/// Internal spawn result.
enum SpawnResult {
    Pid(Pid),
    PidMonitor(Pid, Reference),
}

/// Internal spawn utility.
fn spawn_internal<T>(function: T, link: bool, monitor: bool) -> SpawnResult
where
    T: Future<Output = ()> + Send + 'static,
    T::Output: Send + 'static,
{
    static ID: AtomicU32 = AtomicU32::new(1);

    let mut registry = PROCESS_REGISTRY.write().unwrap();
    let mut next_id = ID.fetch_add(1, Ordering::Relaxed);

    // Guard against spawning more processes than we can allocate ids for.
    if registry.processes.len() >= u32::MAX as usize - 1 {
        drop(registry);
        panic!("Maximum number of processes spawned!");
    }

    loop {
        // Zero is a reserved system process id.
        if next_id == 0 {
            next_id = ID.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        // Make sure the id is lower than the maximum id value, and not already taken.
        if registry.processes.contains_key(&next_id) {
            next_id = ID.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        // We found an unused id.
        break;
    }

    let (tx, rx) = flume::unbounded();

    let pid = Pid::local(next_id);
    let process = Process::new(pid, tx.clone(), rx);

    let mut result = SpawnResult::Pid(pid);

    // If a monitor was requested, insert it before spawning the process.
    if monitor {
        let monitor = Reference::new();

        PROCESS.with(|process| process.monitors.borrow_mut().insert(monitor, pid));

        monitor_create(pid, monitor, Process::current());

        result = SpawnResult::PidMonitor(pid, monitor);
    }

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

    registry.processes.insert(next_id, registration);

    result
}
