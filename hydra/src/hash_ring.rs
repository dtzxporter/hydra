use std::hash::BuildHasher;
use std::hash::Hash;
use std::hash::RandomState;
use std::sync::RwLock;

use hydra_dashmap::DashMap;

/// Internal key, node pair.
struct Node<T> {
    key: u64,
    node: T,
}

impl<T> Node<T> {
    /// Constructs a new instance of [Node].
    pub const fn new(key: u64, node: T) -> Self {
        Self { key, node }
    }
}

/// A concurrent, consistent hash ring.
///
/// Provides the following features:
/// - Lookup optimized ring storage.
/// - Key overrides that allow you to pin a key to a specific node.
///
/// In order to call `find_node`, `T` must implement `Clone`,
/// this is to prevent blocking for too long and keep the ring as up to date as possible.
pub struct HashRing<T, S = RandomState> {
    hash_builder: S,
    ring: RwLock<Vec<Node<T>>>,
    overrides: DashMap<u64, T>,
}

impl<T> HashRing<T> {
    /// Constructs a new instance of [HashRing] with a depth of 1.
    pub fn new() -> Self {
        Self {
            hash_builder: RandomState::new(),
            ring: RwLock::new(Vec::new()),
            overrides: DashMap::new(),
        }
    }

    /// Adds a node to the existing set of nodes in the ring, this will replace an entry if one exists.
    pub fn add_node<K: Hash>(&self, key: K, node: T) -> Option<T> {
        let key = self.hash_key(key);

        let mut ring = self.ring.write().unwrap();

        match ring.binary_search_by(|node| node.key.cmp(&key)) {
            Ok(existing) => {
                return Some(std::mem::replace(&mut ring[existing], Node::new(key, node)).node);
            }
            Err(index) => ring.insert(index, Node::new(key, node)),
        }

        None
    }

    /// Removes a node from the existing set of nodes.
    pub fn remove_node<K: Hash>(&self, key: K) -> Option<T> {
        let key = self.hash_key(key);

        let mut ring = self.ring.write().unwrap();

        if let Ok(existing) = ring.binary_search_by(|node| node.key.cmp(&key)) {
            return Some(ring.remove(existing).node);
        }

        None
    }

    /// Adds a collection of nodes to the existing set of nodes in the ring, replacing any existing entries if they exist.
    pub fn add_nodes<K: Hash, I: IntoIterator<Item = (K, T)>>(&self, nodes: I) {
        let mut ring = self.ring.write().unwrap();

        for (key, node) in nodes.into_iter() {
            let key = self.hash_key(key);

            match ring.binary_search_by(|node| node.key.cmp(&key)) {
                Ok(existing) => {
                    let _ = std::mem::replace(&mut ring[existing], Node::new(key, node));
                }
                Err(index) => ring.insert(index, Node::new(key, node)),
            }
        }
    }

    /// Replaces the nodes in the ring with a new set of nodes.
    pub fn set_nodes<K: Hash, I: IntoIterator<Item = (K, T)>>(&self, nodes: I) {
        let mut ring = self.ring.write().unwrap();

        ring.clear();
        ring.shrink_to_fit();

        drop(ring);

        self.add_nodes(nodes);
    }

    /// Finds the node responsible for the given `key` and passes it to `map`.
    #[inline]
    pub fn map_node<K: Hash, F: FnMut(Option<&T>)>(&self, key: K, mut map: F) {
        let key = self.hash_key(key);

        if let Some(entry) = self.overrides.get(&key) {
            return map(Some(entry.value()));
        }

        let ring = self.ring.read().unwrap();

        if ring.is_empty() {
            return map(None);
        }

        let index = match ring.binary_search_by(|node| node.key.cmp(&key)) {
            Ok(index) => index,
            Err(index) => index,
        };

        if index == ring.len() {
            return map(Some(&ring[0].node));
        }

        map(Some(&ring[index].node));
    }

    /// Adds an override to the ring.
    pub fn add_override<K: Hash>(&self, key: K, node: T) -> Option<T> {
        self.overrides.insert(self.hash_key(key), node)
    }

    /// Adds a collection of overrides to the ring.
    pub fn add_overrides<K: Hash, I: IntoIterator<Item = (K, T)>>(&self, overrides: I) {
        for (key, node) in overrides {
            self.overrides.insert(self.hash_key(key), node);
        }
    }

    /// Replaces the overrides in the ring with a new set of overrides.
    pub fn set_overrides<K: Hash, I: IntoIterator<Item = (K, T)>>(&self, overrides: I) {
        self.overrides.clear();

        for (key, node) in overrides {
            self.overrides.insert(self.hash_key(key), node);
        }
    }

    /// Removes an existing override from the ring.
    pub fn remove_override<K: Hash>(&self, key: K) -> Option<T> {
        self.overrides
            .remove(&self.hash_key(key))
            .map(|entry| entry.1)
    }

    /// Clears all of the nodes and overrides from the hash ring.
    pub fn clear(&self) {
        let mut ring = self.ring.write().unwrap();

        ring.clear();
        ring.shrink_to_fit();

        self.overrides.clear();
        self.overrides.shrink_to_fit();
    }

    /// Hashes the given key using the selected hasher.
    #[inline(always)]
    fn hash_key<K: Hash>(&self, key: K) -> u64 {
        self.hash_builder.hash_one(key)
    }
}

impl<T> HashRing<T>
where
    T: Clone,
{
    /// Returns the node responsible for `key`. If there are no nodes in the ring, it will return `None`.
    pub fn find_node<K: Hash>(&self, key: K) -> Option<T> {
        let mut result: Option<T> = None;

        self.map_node(key, |value| result = value.cloned());

        result
    }
}

impl<T> Default for HashRing<T> {
    fn default() -> Self {
        Self::new()
    }
}
