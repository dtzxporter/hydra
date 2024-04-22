use serde::de::DeserializeOwned;
use serde::Serialize;

// TODO: Once specialization lands, we can make a specialization for Receivable that handles types that are `Deserialize` vs not so that
// We can send any T to a local process and panic at runtime if we try to send a non `Deserialize` type.

/// An object that can be sent or received to/from a process.
pub trait Receivable: Serialize + DeserializeOwned + Send + 'static {}

impl<T> Receivable for T where T: Serialize + DeserializeOwned + Send + 'static {}
