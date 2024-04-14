use serde::Deserialize;
use serde::Serialize;

use std::ops::Deref;
use std::ops::DerefMut;

/// Represents a local object in the current process userspace.
///
/// A [Local] can be used to send any `T: Send + 'static` type to another local process.
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Local<T: Send + 'static>(T);

impl<T> Local<T>
where
    T: Send + 'static,
{
    /// Constructs a new [Local] for the given object in the current process userspace.
    pub const fn new(local: T) -> Self {
        Self(local)
    }

    /// Retrieves the object in the current process userspace.
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Deref for Local<T>
where
    T: Send + 'static,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Local<T>
where
    T: Send + 'static,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Serialize for Local<T>
where
    T: Send + 'static,
{
    fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        panic!("Can't send `Local<T>` to a remote process!")
    }
}

impl<'de, T> Deserialize<'de> for Local<T>
where
    T: Send + 'static,
{
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Err(serde::de::Error::custom(
            "Can't receive `Local<T>` from a remote process!",
        ))
    }
}
