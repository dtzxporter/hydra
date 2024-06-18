use std::hash::BuildHasher;
use std::hash::Hash;
use std::hash::RandomState;
use std::sync::Arc;
use std::sync::Mutex;

use arc_swap::ArcSwap;

use dashmap::DashMap;

/// Internal key, node pair.
#[derive(Clone)]
struct Node<T: Clone> {
    key: u64,
    node: T,
}

impl<T> Node<T>
where
    T: Clone,
{
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
pub struct HashRing<T: Clone, S = RandomState> {
    hash_builder: S,
    ring: ArcSwap<Vec<Node<T>>>,
    ring_lock: Mutex<()>,
    overrides: DashMap<u64, T>,
}

impl<T> HashRing<T>
where
    T: Clone,
{
    /// Constructs a new instance of [HashRing].
    pub fn new() -> Self {
        Self {
            hash_builder: RandomState::new(),
            ring: ArcSwap::new(Arc::new(Vec::new())),
            ring_lock: Mutex::new(()),
            overrides: DashMap::new(),
        }
    }

    /// Adds a node to the existing set of nodes in the ring, this will replace an entry if one exists.
    pub fn add_node<K: Hash>(&self, key: K, node: T) -> Option<T> {
        let key = self.hash_key(key);

        let _ring_lock = self.ring_lock.lock().unwrap();

        let mut new_ring: Vec<_> = self.ring.load().iter().cloned().collect();

        match new_ring.binary_search_by(|node| node.key.cmp(&key)) {
            Ok(existing) => {
                let swapped = std::mem::replace(&mut new_ring[existing], Node::new(key, node));

                self.ring.store(Arc::new(new_ring));

                Some(swapped.node)
            }
            Err(index) => {
                new_ring.insert(index, Node::new(key, node));

                self.ring.store(Arc::new(new_ring));

                None
            }
        }
    }

    /// Removes a node from the existing set of nodes.
    pub fn remove_node<K: Hash>(&self, key: K) -> Option<T> {
        let key = self.hash_key(key);

        let _ring_lock = self.ring_lock.lock().unwrap();

        let mut new_ring: Vec<_> = self.ring.load().iter().cloned().collect();

        if let Ok(existing) = new_ring.binary_search_by(|node| node.key.cmp(&key)) {
            let existing = new_ring.remove(existing);

            self.ring.store(Arc::new(new_ring));

            return Some(existing.node);
        }

        None
    }

    /// Adds a collection of nodes to the existing set of nodes in the ring, replacing any existing entries if they exist.
    pub fn add_nodes<K: Hash, I: IntoIterator<Item = (K, T)>>(&self, nodes: I) {
        let _ring_lock = self.ring_lock.lock().unwrap();

        let mut new_ring: Vec<_> = self.ring.load().iter().cloned().collect();

        for (key, node) in nodes.into_iter() {
            let key = self.hash_key(key);

            match new_ring.binary_search_by(|node| node.key.cmp(&key)) {
                Ok(existing) => {
                    let _ = std::mem::replace(&mut new_ring[existing], Node::new(key, node));
                }
                Err(index) => new_ring.insert(index, Node::new(key, node)),
            }
        }

        self.ring.store(Arc::new(new_ring));
    }

    /// Replaces the nodes in the ring with a new set of nodes.
    pub fn set_nodes<K: Hash, I: IntoIterator<Item = (K, T)>>(&self, nodes: I) {
        let _ring_lock = self.ring_lock.lock().unwrap();

        let nodes = nodes.into_iter();
        let mut new_ring: Vec<Node<T>> = Vec::with_capacity(nodes.size_hint().0);

        for (key, node) in nodes {
            let key = self.hash_key(key);

            match new_ring.binary_search_by(|node| node.key.cmp(&key)) {
                Ok(existing) => {
                    let _ = std::mem::replace(&mut new_ring[existing], Node::new(key, node));
                }
                Err(index) => new_ring.insert(index, Node::new(key, node)),
            }
        }

        self.ring.store(Arc::new(new_ring));
    }

    /// Returns the node responsible for `key`. If there are no nodes in the ring, it will return `None`.
    pub fn find_node<K: Hash>(&self, key: K) -> Option<T> {
        let mut result: Option<T> = None;

        self.map_node(key, |value| result = value.cloned());

        result
    }

    /// Finds the node responsible for the given `key` and passes it to `map`.
    #[inline]
    pub fn map_node<K: Hash, F: FnMut(Option<&T>)>(&self, key: K, mut map: F) {
        let key = self.hash_key(key);

        if let Some(entry) = self.overrides.get(&key) {
            return map(Some(entry.value()));
        }

        let ring = self.ring.load();

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
        let _ring_lock = self.ring_lock.lock().unwrap();

        self.ring.store(Arc::new(Vec::new()))
    }

    /// Hashes the given key using the selected hasher.
    #[inline(always)]
    fn hash_key<K: Hash>(&self, key: K) -> u64 {
        self.hash_builder.hash_one(key)
    }
}

impl<T> Default for HashRing<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}
