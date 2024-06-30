use std::io;

use serde::de::DeserializeOwned;
use serde::Serialize;

/// Serializes a value.
pub fn serialize_value<T: Serialize>(value: &T) -> Vec<u8> {
    rmp_serde::to_vec_named(value).unwrap()
}

/// Deserializes a value.
pub fn deserialize_value<T: DeserializeOwned>(value: &[u8]) -> io::Result<T> {
    rmp_serde::from_slice(value).map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))
}
