use hydra_dashmap::DashMap;

use once_cell::sync::Lazy;

use crate::ProcessItem;
use crate::ProcessSend;
use crate::Reference;

/// A collection of active aliases.
static ALIASES: Lazy<DashMap<u64, Alias>> = Lazy::new(DashMap::new);

/// The state of an active alias.
#[derive(Clone)]
pub struct Alias {
    /// The target process sender.
    pub sender: ProcessSend,
    /// Whether or not this alias is a one-shot alias.
    pub reply: bool,
}

/// Creates an alias for the given process and reference.
pub fn alias_create(sender: ProcessSend, reference: Reference, reply: bool) {
    ALIASES.insert(reference.id(), Alias { sender, reply });
}

/// Destroys an alias for the given reference, returning `true` if the alias existed.
pub fn alias_destroy(reference: Reference) -> bool {
    ALIASES.remove(&reference.id()).is_some()
}

/// Destroys all of the alias for every given reference.
pub fn alias_destroy_all<'a, A: IntoIterator<Item = &'a u64>>(ids: A) {
    for id in ids {
        ALIASES.remove(id);
    }
}

/// Retrieves an alias for the given reference if it's active.
pub fn alias_retrieve(reference: Reference) -> Option<Alias> {
    let mut active: Option<Alias> = None;

    let result = ALIASES.remove_if(&reference.id(), |_, alias| {
        if alias.reply {
            true
        } else {
            active = Some(alias.clone());
            false
        }
    });

    result
        .map(|(_, alias)| {
            let _ = alias
                .sender
                .send(ProcessItem::AliasDeactivated(reference.id()));
            alias
        })
        .or(active)
}
