use dashmap::DashMap;

use once_cell::sync::Lazy;

use serde::Deserialize;
use serde::Serialize;

use crate::ExitReason;
use crate::GenServer;
use crate::Pid;

static REGISTRY: Lazy<DashMap<String, DashMap<RegistryKey, Pid>>> = Lazy::new(DashMap::new);

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
}

impl Registry {
    //
}

impl GenServer for Registry {
    type Message = RegistryMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        Ok(())
    }
}
