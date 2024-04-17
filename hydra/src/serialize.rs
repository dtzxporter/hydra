use serde::Serialize;
use tokio::task_local;

use crate::Node;

task_local! {
    /// The target node for use when serializing node-specific resources.
    pub static SERIALIZE_NODE: Node;
}

/// Serializes a value intended for the given node.
pub fn serialize_for_node<T: Serialize>(value: &T, node: Node) -> Vec<u8> {
    SERIALIZE_NODE.sync_scope(node, move || {
        #[cfg(feature = "json")]
        {
            serde_json::to_vec(value).unwrap()
        }

        #[cfg(not(feature = "json"))]
        {
            let _ = value;
            unimplemented!()
        }
    })
}
