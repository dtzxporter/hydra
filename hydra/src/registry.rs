use std::collections::BTreeMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use serde::Deserialize;
use serde::Serialize;

use crate::CallError;
use crate::Dest;
use crate::ExitReason;
use crate::From;
use crate::GenServer;
use crate::Message;
use crate::Node;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;

/// A local collection of active process registries.
static REGISTRY: Lazy<DashMap<String, DashMap<RegistryKey, Pid>>> = Lazy::new(DashMap::new);

/// A registry key.
#[doc(hidden)]
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegistryKey {
    Integer32(i32),
    Integer64(i64),
    Integer128(i128),
    UInteger32(u32),
    UInteger64(u64),
    UInteger128(u128),
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
    AlreadyStarted,
}

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
    lookup: BTreeMap<Pid, RegistryKey>,
}

impl Registry {
    /// Constructs a new local [Registry] with the given name.
    ///
    /// Names must be unique on a per-node basis.
    pub fn new<T: Into<String>>(name: T) -> Self {
        Self {
            name: name.into(),
            start: None,
            lookup: BTreeMap::new(),
        }
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

    /// Looks up, or starts a process by the given key.
    async fn lookup_or_start_by_key(&mut self, key: RegistryKey) -> Result<Pid, RegistryError> {
        if let Some(result) = lookup_process(&self.name, &key) {
            return Ok(result);
        }

        let start_child = Pin::from(self.start.as_ref().unwrap()(key.clone())).await;

        match start_child {
            Ok(pid) => {
                #[cfg(feature = "tracing")]
                tracing::info!(child_name = ?key, child_pid = ?pid, "Started registered process");

                self.lookup.insert(pid, key.clone());

                register_process(&self.name, key, pid);

                Ok(pid)
            }
            Err(reason) => {
                #[cfg(feature = "tracing")]
                tracing::error!(reason = ?reason, child_name = ?key, "Start registered process error");

                Err(RegistryError::StartError(reason))
            }
        }
    }

    /// Looks up a process by the given key.
    fn lookup_by_key(&mut self, key: RegistryKey) -> Option<Pid> {
        lookup_process(&self.name, &key)
    }

    /// Removes the process from the registry.
    fn remove_process(&mut self, pid: Pid) {
        let Some(key) = self.lookup.remove(&pid) else {
            return;
        };

        REGISTRY.alter(&self.name, |_, value| {
            value.remove_if(&key, |_, value| *value == pid);
            value
        });
    }
}

impl GenServer for Registry {
    type Message = RegistryMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        Process::set_flags(ProcessFlags::TRAP_EXIT);

        Ok(())
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
            _ => unreachable!(),
        }
    }

    async fn handle_info(&mut self, info: Message<Self::Message>) -> Result<(), ExitReason> {
        match info {
            Message::System(SystemMessage::Exit(pid, _)) => {
                self.remove_process(pid);
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

/// Registers the given process in the local registry with the given key.
fn register_process<T: Into<String>>(registry: T, key: RegistryKey, process: Pid) {
    REGISTRY
        .entry(registry.into())
        .or_default()
        .insert(key, process);
}
