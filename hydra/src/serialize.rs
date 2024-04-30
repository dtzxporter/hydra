use std::io;

use serde::de::DeserializeOwned;
use serde::Serialize;

/// Serializes a value.
pub fn serialize_value<T: Serialize>(value: &T) -> Vec<u8> {
    #[cfg(feature = "json")]
    {
        serde_json::to_vec(value).unwrap()
    }

    #[cfg(not(feature = "json"))]
    {
        let _ = value;
        panic!("You must pick a feature to use for serialization!")
    }
}

/// Deserializes a value.
pub fn deserialize_value<T: DeserializeOwned>(value: &[u8]) -> io::Result<T> {
    #[cfg(feature = "json")]
    {
        serde_json::from_slice(value)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))
    }

    #[cfg(not(any(feature = "json")))]
    {
        let _ = value;
        panic!("You must pick a feature to use for serialization!")
    }
}
