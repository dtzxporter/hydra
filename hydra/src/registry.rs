use std::collections::BTreeMap;
use std::future::Future;
use std::sync::Arc;

use dashmap::DashMap;

use once_cell::sync::Lazy;

use serde::Deserialize;
use serde::Serialize;

use crate::ExitReason;
use crate::GenServer;
use crate::Message;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;

static REGISTRY: Lazy<DashMap<String, DashMap<RegistryKey, Pid>>> = Lazy::new(DashMap::new);

/// A registry key.
#[doc(hidden)]
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegistryKey {
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    String(String),
}

/// A registry message.
#[doc(hidden)]
#[derive(Serialize, Deserialize)]
pub enum RegistryMessage {
    //
}

pub struct Registry {
    name: String,
    #[allow(clippy::type_complexity)]
    start: Option<
        Arc<
            dyn Fn() -> Box<dyn Future<Output = Result<Pid, ExitReason>> + Send + Sync>
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

impl From<i32> for RegistryKey {
    fn from(value: i32) -> Self {
        Self::I32(value)
    }
}

impl From<i64> for RegistryKey {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<u32> for RegistryKey {
    fn from(value: u32) -> Self {
        Self::U32(value)
    }
}

impl From<u64> for RegistryKey {
    fn from(value: u64) -> Self {
        Self::U64(value)
    }
}

impl From<String> for RegistryKey {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for RegistryKey {
    fn from(value: &str) -> Self {
        Self::String(value.to_owned())
    }
}
