use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;

use flume::Receiver;
use flume::Sender;

use crate::alias_create;
use crate::alias_destroy;
use crate::alias_destroy_all;
use crate::link_create;
use crate::link_destroy;
use crate::link_install;
use crate::link_process_down;
use crate::monitor_create;
use crate::monitor_destroy;
use crate::monitor_destroy_all;
use crate::monitor_install;
use crate::monitor_process_down;
use crate::node_process_send_exit;
use crate::process_alive;
use crate::process_destroy_timer;
use crate::process_drop;
use crate::process_exit;
use crate::process_flags;
use crate::process_insert;
use crate::process_list;
use crate::process_name_list;
use crate::process_name_lookup;
use crate::process_name_remove;
use crate::process_read_timer;
use crate::process_register;
use crate::process_register_timer;
use crate::process_send;
use crate::process_set_exit_reason;
use crate::process_set_flags;
use crate::process_unregister;
use crate::ArgumentError;
use crate::AsyncCatchUnwind;
use crate::Dest;
use crate::Dests;
use crate::ExitReason;
use crate::Message;
use crate::Pid;
use crate::ProcessFlags;
use crate::ProcessItem;
use crate::ProcessMonitor;
use crate::ProcessReceiver;
use crate::ProcessRegistration;
use crate::Receivable;
use crate::Reference;
use crate::Timeout;

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
    pub(crate) monitors: RefCell<BTreeMap<Reference, ProcessMonitor>>,
}

tokio::task_local! {
    /// Current process information.
    pub(crate) static PROCESS: Process;
}

impl Process {
    /// Constructs a new [Process] from the given [Pid] and channels.
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
        process_name_lookup(name.as_ref())
    }

    /// Sends a message to `dests`.
    ///
    /// ## Example:
    /// This method allows sending to multiple targets with certain trade offs:
    /// ```ignore
    /// let pid1 = Process::spawn(async { /* */ });
    /// let pid2 = Process::spawn(async { /* */ });
    ///
    /// // faster when all processes are local.
    /// for pid in &[pid1, pid2] {
    ///     Process::send(pid, "hello world!");
    /// }
    ///
    /// // faster when processes are mostly remote.
    /// Process::send(&[pid1, pid2], "hello world!");
    /// ```
    pub fn send<D: Into<Dests>, M: Receivable>(dests: D, message: M) {
        process_send(dests.into(), message);
    }

    /// Sends a message to `dests` after the given `duration`.
    ///
    /// See [Process::send] for performance trade-offs.
    ///
    /// ## Example:
    /// Sends a message after 5 seconds to `pid`:
    /// ```ignore
    /// Process::send_after(pid, "hello world!", Duration::from_secs(5));
    /// ```
    pub fn send_after<D: Into<Dests>, M: Receivable>(
        dest: D,
        message: M,
        duration: Duration,
    ) -> Reference {
        let dest = dest.into();

        let reference = Reference::new();

        let handle = tokio::spawn(async move {
            Process::sleep(duration).await;
            Process::send(dest, message);

            process_destroy_timer(reference);
        });

        process_register_timer(reference, duration, handle);

        reference
    }

    /// Cancels a timer created by `send_after`.
    pub fn cancel_timer(timer: Reference) {
        process_destroy_timer(timer);
    }

    /// Reads a timer created by `send_after`.
    ///
    /// It returns the time remaining until the timer expires.
    pub fn read_timer(timer: Reference) -> Option<Duration> {
        process_read_timer(timer)
    }

    /// Creates a receiver with advanced filtering options from the current processes mailbox.
    #[must_use]
    pub fn receiver() -> ProcessReceiver<()> {
        ProcessReceiver::new()
    }

    /// Creates a receiver for a single message that matches the given type from the current processes mailbox.
    ///
    /// This will panic if a message is received that doesn't match the given type.
    #[must_use]
    pub async fn receive<T: Receivable>() -> Message<T> {
        ProcessReceiver::new()
            .strict_type_checking()
            .receive()
            .await
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
        process_alive(pid)
    }

    /// Sleeps the current process for the given duration.
    pub async fn sleep(duration: Duration) {
        tokio::time::sleep(duration).await
    }

    /// Waits for the given future to complete until the given duration is up.
    pub async fn timeout<F>(duration: Duration, future: F) -> Result<<F as Future>::Output, Timeout>
    where
        F: Future,
    {
        pingora_timeout::timeout(duration, future)
            .await
            .map_err(|_| Timeout)
    }

    /// Registers the given [Pid] under `name` if the process is local, active, and the name is not already registered.
    pub fn register<S: Into<String>>(pid: Pid, name: S) -> Result<(), ArgumentError> {
        process_register(pid, name.into())
    }

    /// Removes the registered `name`, associated with a [Pid].
    pub fn unregister<S: AsRef<str>>(name: S) {
        process_unregister(name.as_ref());
    }

    /// Returns a [Vec] of registered process names.
    #[must_use]
    pub fn registered() -> Vec<String> {
        process_name_list()
    }

    /// Returns a [Vec] of [Pid]'s on the local node.
    #[must_use]
    pub fn list() -> Vec<Pid> {
        process_list()
    }

    /// Creates a bi-directional link between the current process and the given process.
    pub fn link(pid: Pid) {
        let current = Self::current();

        if pid == current {
            return;
        }

        link_install(pid, current);
    }

    /// Removes the link between the calling process and the given process.
    pub fn unlink(pid: Pid) {
        let current = Self::current();

        if pid == current {
            return;
        }

        link_destroy(pid, current);
    }

    /// Starts monitoring the given process from the calling process. If the process is already dead a message is sent immediately.
    pub fn monitor<T: Into<Dest>>(process: T) -> Reference {
        let current = Self::current();
        let process = process.into();

        let reference = Reference::new();

        monitor_install(process, reference, current);

        reference
    }

    /// Starts monitoring the given process from the calling process. If the process is already dead a message is sent immediately.
    ///
    /// Creates an alias for the calling process that's tied to the process monitor.
    ///
    /// The alias will be deactivated if:
    /// - The monitor sends a down message.
    /// - The user explicitly calls `unalias`. (The monitor will remain active)
    /// - `reply` is `true` and a message is sent over the alias.
    pub fn monitor_alias<T: Into<Dest>>(process: T, reply: bool) -> Reference {
        let current = Self::current();
        let process = process.into();
        let sender = PROCESS.with(|process| process.sender.clone());

        let reference = Reference::new();

        alias_create(sender, reference, reply);

        monitor_install(process, reference, current);

        reference
    }

    /// Demonitors the monitor identified by the given reference.
    ///
    /// If a monitor message was sent to the process already but was not received, it will be discarded automatically.
    pub fn demonitor(monitor: Reference) {
        let Some(process_monitor) =
            PROCESS.with(|process| process.monitors.borrow_mut().remove(&monitor))
        else {
            return;
        };

        let ProcessMonitor::ForProcess(pid) = process_monitor else {
            panic!("Invalid process monitor reference!");
        };

        let Some(pid) = pid else {
            return;
        };

        monitor_destroy(pid, monitor);

        alias_destroy(monitor);
    }

    /// Returns the current process flags.
    #[must_use]
    pub fn flags() -> ProcessFlags {
        process_flags(Self::current()).unwrap()
    }

    /// Sets one or more process flags.
    pub fn set_flags(flags: ProcessFlags) {
        process_set_flags(Self::current(), flags)
    }

    /// Sends an exit signal with the given reason to [Pid].
    pub fn exit<E: Into<ExitReason>>(pid: Pid, exit_reason: E) {
        let exit_reason = exit_reason.into();

        if pid.is_local() {
            process_exit(pid, Self::current(), exit_reason);
        } else {
            node_process_send_exit(pid, Self::current(), exit_reason);
        }
    }
}

impl Drop for Process {
    fn drop(&mut self) {
        let process = process_drop(self.pid).unwrap();

        if let Some(name) = process.name {
            process_name_remove(&name);
        }

        let exit_reason = process.exit_reason.unwrap_or_default();

        link_process_down(self.pid, exit_reason.clone());

        monitor_process_down(self.pid, exit_reason);
        monitor_destroy_all(self.monitors.borrow().iter());

        alias_destroy_all(self.aliases.borrow().iter());
    }
}

/// Internal spawn result.
enum SpawnResult {
    Pid(Pid),
    PidMonitor(Pid, Reference),
}

/// The next process id to allocate if free.
static ID: AtomicU64 = AtomicU64::new(1);

/// Internal spawn utility.
fn spawn_internal<T>(function: T, link: bool, monitor: bool) -> SpawnResult
where
    T: Future<Output = ()> + Send + 'static,
    T::Output: Send + 'static,
{
    let next_id = ID.fetch_add(1, Ordering::Relaxed);

    let (tx, rx) = flume::unbounded();

    let pid = Pid::local(next_id);
    let process = Process::new(pid, tx.clone(), rx);

    let mut result = SpawnResult::Pid(pid);

    // If a link was requested, insert it before spawning the process.
    if link {
        let current = Process::current();

        link_create(pid, current, true);
        link_create(current, pid, true);
    }

    // If a monitor was requested, insert it before spawning the process.
    if monitor {
        let monitor = Reference::new();

        PROCESS.with(|process| {
            process
                .monitors
                .borrow_mut()
                .insert(monitor, ProcessMonitor::ForProcess(Some(pid)))
        });

        monitor_create(pid, monitor, Process::current(), Some(pid.into()));

        result = SpawnResult::PidMonitor(pid, monitor);
    }

    // Spawn the process with the newly created process object in scope.
    let handle = tokio::spawn(PROCESS.scope(process, async move {
        if let Err(e) = AsyncCatchUnwind::new(AssertUnwindSafe(function)).await {
            process_set_exit_reason(Process::current(), e.into());
        }
    }));

    // Register the process under it's new id.
    process_insert(next_id, ProcessRegistration::new(handle, tx));

    result
}
