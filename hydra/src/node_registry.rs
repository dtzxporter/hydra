use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::Pid;

/// Maps node id to the process responsible for the node.
pub static NODE_REGISTRY: Lazy<RwLock<HashMap<u64, Pid>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// A secret value that secures the connection between nodes.
pub static NODE_COOKIE: Mutex<Option<String>> = Mutex::new(None);
