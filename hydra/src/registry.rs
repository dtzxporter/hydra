use std::collections::BTreeMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use serde::Deserialize;
use serde::Serialize;

use crate::shutdown_infinity;
use crate::shutdown_timeout;
use crate::CallError;
use crate::ChildSpec;
use crate::ChildType;
use crate::Dest;
use crate::ExitReason;
use crate::From;
use crate::GenServer;
use crate::Message;
use crate::Node;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::Reference;
use crate::RegistryOptions;
use crate::Shutdown;
use crate::SystemMessage;

/// A local collection of active process registries.
static REGISTRY: Lazy<DashMap<String, DashMap<RegistryKey, Pid>>> = Lazy::new(DashMap::new);

/// A registry key.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegistryKey {
    /// A 32bit signed integer key.
    Integer32(i32),
    /// A 64bit signed integer key.
    Integer64(i64),
    /// A 128bit signed integer key.
    Integer128(i128),
    /// A 32bit unsigned integer key.
    UInteger32(u32),
    /// A 64bit unsigned integer key.
    UInteger64(u64),
    /// A 128bit unsigned integer key.
    UInteger128(u128),
    /// A string key.
    String(String),
}

/// A registry message.
#[doc(hidden)]
#[derive(Serialize, Deserialize)]
pub enum RegistryMessage {
    Lookup(RegistryKey),
    LookupSuccess(Option<Pid>),
    LookupOrStart(RegistryKey),
    LookupOrStartSuccess(Pid),
    LookupOrStartError(RegistryError),
    Start(RegistryKey),
    StartSuccess(Pid),
    StartError(RegistryError),
    Stop(RegistryKey),
    StopSuccess,
    StopError(RegistryError),
    Count,
    CountSuccess(usize),
    List,
    ListSuccess(Vec<(RegistryKey, Pid)>),
    Remove(RegistryKey),
    RemoveSuccess(Option<Pid>),
    RemoveLookup(Pid),
}

/// Errors for [Registry] calls.
#[derive(Debug, Serialize, Deserialize)]
pub enum RegistryError {
    /// A call to the [Registry] server has failed.
    CallError(CallError),
    /// The process failed to start.
    StartError(ExitReason),
    /// The registry wasn't configured with a start routine.
    StartNotSupported,
    /// The process is already registered and running.
    AlreadyStarted(Pid),
    /// The process was not found for the given key.
    NotFound,
}

/// Provides a centralized 'registry' of processes using any value as a key.
///
/// A registry is designed to only support a single type of [Process] or [GenServer], you should use multiple if necessary.
#[derive(Clone)]
pub struct Registry {
    name: String,
    #[allow(clippy::type_complexity)]
    start: Option<
        Arc<
            dyn Fn(RegistryKey) -> Box<dyn Future<Output = Result<Pid, ExitReason>> + Send + Sync>
                + Send
                + Sync,
        >,
    >,
    shutdown: Shutdown,
    lookup: BTreeMap<Pid, RegistryKey>,
}

impl Registry {
    /// Constructs a new local [Registry] with the given name.
    ///
    /// Names must be unique on a per-node basis.
    #[must_use]
    pub fn new<T: Into<String>>(name: T) -> Self {
        Self {
            name: name.into(),
            start: None,
            shutdown: Shutdown::BrutalKill,
            lookup: BTreeMap::new(),
        }
    }

    /// Assigns a start routine for the registry to be able to dynamically spawn processes.
    ///
    /// Must return a future that resolves to [Result<Pid, ExitReason>].
    #[must_use]
    pub fn with_start<T, F>(mut self, start: T) -> Self
    where
        T: Fn(RegistryKey) -> F + Send + Sync + 'static,
        F: Future<Output = Result<Pid, ExitReason>> + Send + Sync + 'static,
    {
        self.start = Some(Arc::new(move |key| Box::new(start(key))));
        self
    }

    /// Sets the method used to shutdown any registered processes once when the [Registry] is terminated.
    pub fn with_shutdown(mut self, shutdown: Shutdown) -> Self {
        self.shutdown = shutdown;
        self
    }

    /// Looks up a running process.
    ///
    /// If the registry is local, this will just query the table without hitting the registry process.
    pub async fn lookup<T: Into<Dest>, N: Into<RegistryKey>>(
        registry: T,
        key: N,
    ) -> Result<Option<Pid>, RegistryError> {
        use RegistryMessage::*;

        let registry = registry.into();
        let key = key.into();

        if let Dest::Named(registry, Node::Local) = &registry {
            return Ok(lookup_process(registry, &key));
        }

        match Registry::call(registry, Lookup(key), None).await? {
            LookupSuccess(pid) => Ok(pid),
            _ => unreachable!(),
        }
    }

    /// Attempts to lookup a running process.
    ///
    /// If the process isn't currently running, it is spawned and passed the key.
    pub async fn lookup_or_start<T: Into<Dest>, N: Into<RegistryKey>>(
        registry: T,
        key: N,
    ) -> Result<Pid, RegistryError> {
        use RegistryMessage::*;

        let registry = registry.into();
        let key = key.into();

        if let Dest::Named(registry, Node::Local) = &registry {
            if let Some(result) = lookup_process(registry, &key) {
                return Ok(result);
            }
        }

        match Registry::call(registry, LookupOrStart(key), None).await? {
            LookupOrStartSuccess(pid) => Ok(pid),
            LookupOrStartError(error) => Err(error),
            _ => unreachable!(),
        }
    }

    /// Attempts to start a process.
    ///
    /// If the process is already running, an error is returned.
    pub async fn start_process<T: Into<Dest>, N: Into<RegistryKey>>(
        registry: T,
        key: N,
    ) -> Result<Pid, RegistryError> {
        use RegistryMessage::*;

        match Registry::call(registry, Start(key.into()), None).await? {
            StartSuccess(pid) => Ok(pid),
            StartError(error) => Err(error),
            _ => unreachable!(),
        }
    }

    /// Stops a process registered in the given `registry` with the given `key`.
    ///
    /// If the process is trapping exits, it will still run, but be unregistered from this registry.
    ///
    /// If the process is not registered an error is returned.
    pub async fn stop_process<T: Into<Dest>, N: Into<RegistryKey>>(
        registry: T,
        key: N,
    ) -> Result<(), RegistryError> {
        use RegistryMessage::*;

        match Registry::call(registry, Stop(key.into()), None).await? {
            StopSuccess => Ok(()),
            StopError(error) => Err(error),
            _ => unreachable!(),
        }
    }

    /// Returns the number of registered processes for the given `registry`.
    pub async fn count<T: Into<Dest>>(registry: T) -> Result<usize, RegistryError> {
        use RegistryMessage::*;

        let registry = registry.into();

        if let Dest::Named(registry, Node::Local) = &registry {
            return Ok(count_processes(registry));
        }

        match Registry::call(registry, Count, None).await? {
            CountSuccess(count) => Ok(count),
            _ => unreachable!(),
        }
    }

    /// Returns a list of every process registered to the given `registry`.
    ///
    /// There is no ordering guarantee.
    pub async fn list<T: Into<Dest>>(
        registry: T,
    ) -> Result<Vec<(RegistryKey, Pid)>, RegistryError> {
        use RegistryMessage::*;

        let registry = registry.into();

        if let Dest::Named(registry, Node::Local) = &registry {
            return Ok(list_processes(registry));
        }

        match Registry::call(registry, List, None).await? {
            ListSuccess(list) => Ok(list),
            _ => unreachable!(),
        }
    }

    /// Removes a process registered in the given `registry` with the given `key`.
    ///
    /// The process will no longer be registered, but it will remain running if it was found.
    pub async fn remove<T: Into<Dest>, N: Into<RegistryKey>>(
        registry: T,
        key: N,
    ) -> Result<Option<Pid>, RegistryError> {
        use RegistryMessage::*;

        let registry = registry.into();
        let key = key.into();

        if let Dest::Named(registry, Node::Local) = &registry {
            let Some(process) = remove_process(registry, &key) else {
                return Ok(None);
            };

            Registry::cast(registry.to_string(), RemoveLookup(process));

            return Ok(Some(process));
        }

        match Registry::call(registry, Remove(key), None).await? {
            RemoveSuccess(pid) => Ok(pid),
            _ => unreachable!(),
        }
    }

    /// Create a registry proces not linked to a supervision tree.
    pub async fn start(self, mut options: RegistryOptions) -> Result<Pid, ExitReason> {
        if options.name.is_none() {
            options = options.name(self.name.clone());
        }

        GenServer::start(self, options.into()).await
    }

    /// Creates a registry process as part of a supervision tree.
    ///
    /// For example, this function ensures that the registry is linked to the calling process (its supervisor).
    pub async fn start_link(self, mut options: RegistryOptions) -> Result<Pid, ExitReason> {
        if options.name.is_none() {
            options = options.name(self.name.clone());
        }

        GenServer::start_link(self, options.into()).await
    }

    /// Builds a child specification for this [Registry] process.
    pub fn child_spec(self, mut options: RegistryOptions) -> ChildSpec {
        if options.name.is_none() {
            options = options.name(self.name.clone());
        }

        ChildSpec::new("Registry")
            .start(move || self.clone().start_link(options.clone()))
            .child_type(ChildType::Supervisor)
    }

    /// Looks up, or starts a process by the given key.
    async fn lookup_or_start_by_key(&mut self, key: RegistryKey) -> Result<Pid, RegistryError> {
        if let Some(result) = lookup_process(&self.name, &key) {
            return Ok(result);
        }

        self.start_by_key(key).await
    }

    /// Starts a process if one doesn't exist.
    async fn start_by_key(&mut self, key: RegistryKey) -> Result<Pid, RegistryError> {
        if let Some(process) = lookup_process(&self.name, &key) {
            return Err(RegistryError::AlreadyStarted(process));
        }

        let start_child = Pin::from(self.start.as_ref().unwrap()(key.clone())).await;

        match start_child {
            Ok(pid) => {
                #[cfg(feature = "tracing")]
                tracing::info!(child_key = ?key, child_pid = ?pid, "Started registered process");

                self.lookup.insert(pid, key.clone());

                register_process(&self.name, key, pid);

                Ok(pid)
            }
            Err(reason) => {
                #[cfg(feature = "tracing")]
                tracing::error!(reason = ?reason, child_key = ?key, "Start registered process error");

                Err(RegistryError::StartError(reason))
            }
        }
    }

    /// Terminates all registered children of this registry.
    async fn terminate_children(&mut self) {
        match self.shutdown {
            Shutdown::BrutalKill => {
                // Do nothing, the drop will automatically kill each process.
            }
            _ => {
                let Some((_, registry)) = REGISTRY.remove(&self.name) else {
                    return;
                };

                let mut monitors: Vec<(Pid, Reference)> = Vec::with_capacity(registry.len());

                for (process, key) in &self.lookup {
                    if registry.contains_key(key) {
                        monitors.push((*process, Process::monitor(*process)));

                        Process::exit(*process, ExitReason::from("shutdown"));
                    }
                }

                for (process, monitor) in monitors {
                    if let Shutdown::Duration(timeout) = self.shutdown {
                        let _ = shutdown_timeout(process, monitor, timeout).await;
                    } else if let Shutdown::Infinity = self.shutdown {
                        let _ = shutdown_infinity(process, monitor).await;
                    }
                }

                self.lookup.clear();
            }
        }
    }

    /// Stops a process by the given key.
    fn stop_by_key(&mut self, key: RegistryKey) -> Result<(), RegistryError> {
        let Some(process) = lookup_process(&self.name, &key) else {
            return Err(RegistryError::NotFound);
        };

        Process::unlink(process);
        Process::exit(process, ExitReason::from("shutdown"));

        self.lookup.remove(&process);

        remove_process(&self.name, &key);

        Ok(())
    }

    /// Looks up a process by the given key.
    fn lookup_by_key(&mut self, key: RegistryKey) -> Option<Pid> {
        lookup_process(&self.name, &key)
    }

    /// Removes a process by the given key.
    fn remove_by_key(&mut self, key: RegistryKey) -> Option<Pid> {
        let process = remove_process(&self.name, &key)?;

        Process::unlink(process);

        self.lookup.remove(&process);

        Some(process)
    }

    /// Removes the process from the registry.
    fn remove_process(&mut self, pid: Pid, reason: ExitReason) {
        let Some(key) = self.lookup.remove(&pid) else {
            return;
        };

        #[cfg(feature = "tracing")]
        tracing::info!(reason = ?reason, child_key = ?key, child_pid = ?pid, "Removed registered process");

        #[cfg(not(feature = "tracing"))]
        let _ = reason;

        REGISTRY.alter(&self.name, |_, value| {
            value.remove_if(&key, |_, value| *value == pid);
            value
        });
    }
}

impl Drop for Registry {
    fn drop(&mut self) {
        REGISTRY.remove(&self.name);

        for process in self.lookup.keys() {
            Process::unlink(*process);
            Process::exit(*process, ExitReason::Kill);
        }
    }
}

impl GenServer for Registry {
    type Message = RegistryMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        Ok(())
    }

    async fn terminate(&mut self, _reason: ExitReason) {
        self.terminate_children().await;
    }

    async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
        use RegistryMessage::*;

        match message {
            RemoveLookup(process) => {
                Process::unlink(process);

                self.lookup.remove(&process);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        use RegistryMessage::*;

        match message {
            Lookup(key) => {
                let result = self.lookup_by_key(key);

                Ok(Some(LookupSuccess(result)))
            }
            LookupOrStart(key) => match self.lookup_or_start_by_key(key).await {
                Ok(pid) => Ok(Some(LookupOrStartSuccess(pid))),
                Err(error) => Ok(Some(LookupOrStartError(error))),
            },
            Start(key) => match self.start_by_key(key).await {
                Ok(pid) => Ok(Some(StartSuccess(pid))),
                Err(error) => Ok(Some(StartError(error))),
            },
            Stop(key) => match self.stop_by_key(key) {
                Ok(()) => Ok(Some(StopSuccess)),
                Err(error) => Ok(Some(StopError(error))),
            },
            Count => {
                let count = count_processes(&self.name);

                Ok(Some(CountSuccess(count)))
            }
            List => {
                let list = list_processes(&self.name);

                Ok(Some(ListSuccess(list)))
            }
            Remove(key) => {
                let removed = self.remove_by_key(key);

                Ok(Some(RemoveSuccess(removed)))
            }
            _ => unreachable!(),
        }
    }

    async fn handle_info(&mut self, info: Message<Self::Message>) -> Result<(), ExitReason> {
        match info {
            Message::System(SystemMessage::Exit(pid, reason)) => {
                self.remove_process(pid, reason);
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

impl std::convert::From<i32> for RegistryKey {
    fn from(value: i32) -> Self {
        Self::Integer32(value)
    }
}

impl std::convert::From<i64> for RegistryKey {
    fn from(value: i64) -> Self {
        Self::Integer64(value)
    }
}

impl std::convert::From<i128> for RegistryKey {
    fn from(value: i128) -> Self {
        Self::Integer128(value)
    }
}

impl std::convert::From<u32> for RegistryKey {
    fn from(value: u32) -> Self {
        Self::UInteger32(value)
    }
}

impl std::convert::From<u64> for RegistryKey {
    fn from(value: u64) -> Self {
        Self::UInteger64(value)
    }
}

impl std::convert::From<u128> for RegistryKey {
    fn from(value: u128) -> Self {
        Self::UInteger128(value)
    }
}

impl std::convert::From<String> for RegistryKey {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl std::convert::From<&str> for RegistryKey {
    fn from(value: &str) -> Self {
        Self::String(value.to_owned())
    }
}

impl std::convert::From<CallError> for RegistryError {
    fn from(value: CallError) -> Self {
        Self::CallError(value)
    }
}

/// Looks up a process in the given registry assigned to the given key.
fn lookup_process<T: AsRef<str>>(registry: T, key: &RegistryKey) -> Option<Pid> {
    REGISTRY
        .get(registry.as_ref())
        .and_then(|registry| registry.get(key).map(|entry| *entry.value()))
}

/// Removes a process in the given registry assigned to the given key.
fn remove_process<T: AsRef<str>>(registry: T, key: &RegistryKey) -> Option<Pid> {
    REGISTRY
        .get_mut(registry.as_ref())
        .and_then(|registry| registry.remove(key).map(|entry| entry.1))
}

/// Counts the number of processes in a registry.
fn count_processes<T: AsRef<str>>(registry: T) -> usize {
    REGISTRY
        .get(registry.as_ref())
        .map(|registry| registry.len())
        .unwrap_or_default()
}

/// Lists all of the processes in a registry.
fn list_processes<T: AsRef<str>>(registry: T) -> Vec<(RegistryKey, Pid)> {
    REGISTRY
        .get(registry.as_ref())
        .map(|registry| {
            registry
                .iter()
                .map(|entry| (entry.key().clone(), *entry.value()))
                .collect()
        })
        .unwrap_or_default()
}

/// Registers the given process in the local registry with the given key.
fn register_process<T: Into<String>>(registry: T, key: RegistryKey, process: Pid) {
    REGISTRY
        .entry(registry.into())
        .or_default()
        .insert(key, process);
}
