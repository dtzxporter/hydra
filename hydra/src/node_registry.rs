use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::Pid;

/// Maps node id to the process responsible for the node.
pub static NODE_REGISTRY: Lazy<RwLock<HashMap<u64, Pid>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

// When a pid is serialized over the wire, we need to lookup it's node@ip:port combination.
// If it's already in the registry, we need to get it's node id, else
// we need to get it's thing.

/// A secret value that secures the connection between nodes.
pub static NODE_COOKIE: Mutex<Option<String>> = Mutex::new(None);
