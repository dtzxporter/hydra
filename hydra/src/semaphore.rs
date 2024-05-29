use std::sync::Arc;

use tokio::sync::OwnedSemaphorePermit as OwnedSemaphoreBasePermit;
use tokio::sync::Semaphore as SemaphoreBase;
use tokio::sync::SemaphorePermit as SemaphoreBasePermit;

/// Represents a permit to a [Semaphore].
pub struct SemaphorePermit<'a> {
    _permit: SemaphoreBasePermit<'a>,
}

/// Represents an owned permit to a [Semaphore].
pub struct OwnedSemaphorePermit {
    _permit: OwnedSemaphoreBasePermit,
}

/// Error when there are no permits available.
#[derive(Debug)]
pub struct NoPermits;

/// A semaphore maintains a set of permits. Permits are used to synchronize access to a shared resource.
/// A semaphore differs from a mutex in that it can allow more than one concurrent caller to access the shared resource at a time.
#[repr(transparent)]
pub struct Semaphore {
    inner: SemaphoreBase,
}

/// An owned semaphore is identical to a [Semaphore] except it can be owned by an actor and it's permits can still be shared externally.
#[repr(transparent)]
pub struct OwnedSemaphore {
    inner: Arc<SemaphoreBase>,
}

impl Semaphore {
    /// Creates a new instance of [Semaphore] with the given `permits` count.
    ///
    /// This is typically used to create a static semaphore:
    /// ```
    /// use hydra::Semaphore;
    ///
    /// static RATE_LIMIT: Semaphore = Semaphore::new(100);
    /// ```
    pub const fn new(permits: usize) -> Self {
        Self {
            inner: SemaphoreBase::const_new(permits),
        }
    }

    /// Acquires one permit asynchronously waiting for one to become available.
    #[must_use]
    pub async fn acquire(&self) -> SemaphorePermit {
        let permit = self.inner.acquire().await.unwrap();

        SemaphorePermit { _permit: permit }
    }

    /// Acquires many permits asynchronously waiting for them to become available.
    #[must_use]
    pub async fn acquire_many(&self, count: u32) -> SemaphorePermit {
        let permit = self.inner.acquire_many(count).await.unwrap();

        SemaphorePermit { _permit: permit }
    }

    /// Attempts to acquire a permit, returning an error if there are none available.
    pub fn try_acquire(&self) -> Result<SemaphorePermit, NoPermits> {
        let permit = self.inner.try_acquire().map_err(|_| NoPermits)?;

        Ok(SemaphorePermit { _permit: permit })
    }
}

impl OwnedSemaphore {
    /// Creates a new instance of [OwnedSemaphore] with the given `permits` count that can be used with owned permits.
    ///
    /// This can be used to create a owned semaphore that lives in a state:
    /// ```
    /// use hydra::OwnedSemaphore;
    ///
    /// struct MyServer {
    ///     rate_limit: OwnedSemaphore,
    /// }
    ///
    /// impl MyServer {
    ///     pub fn new() -> Self {
    ///         Self {
    ///             rate_limit: OwnedSemaphore::new(100),
    ///         }
    ///     }
    /// }
    /// ```
    pub fn new(permits: usize) -> Self {
        Self {
            inner: Arc::new(SemaphoreBase::new(permits)),
        }
    }

    /// Acquires one permit asynchronously waiting for one to become available.
    #[must_use]
    pub async fn acquire(&self) -> SemaphorePermit {
        let permit = self.inner.acquire().await.unwrap();

        SemaphorePermit { _permit: permit }
    }

    /// Acquires one permit asynchronously waiting for one to become available.
    #[must_use]
    pub async fn acquire_owned(&self) -> OwnedSemaphorePermit {
        let permit = self.inner.clone().acquire_owned().await.unwrap();

        OwnedSemaphorePermit { _permit: permit }
    }

    /// Acquires many permits asynchronously waiting for them to become available.
    #[must_use]
    pub async fn acquire_many(&self, count: u32) -> SemaphorePermit {
        let permit = self.inner.acquire_many(count).await.unwrap();

        SemaphorePermit { _permit: permit }
    }

    /// Acquires many permits asynchronously waiting for them to become available.
    #[must_use]
    pub async fn acquire_many_owned(&self, count: u32) -> OwnedSemaphorePermit {
        let permit = self.inner.clone().acquire_many_owned(count).await.unwrap();

        OwnedSemaphorePermit { _permit: permit }
    }

    /// Attempts to acquire a permit, returning an error if there are none available.
    pub fn try_acquire(&self) -> Result<SemaphorePermit, NoPermits> {
        let permit = self.inner.try_acquire().map_err(|_| NoPermits)?;

        Ok(SemaphorePermit { _permit: permit })
    }

    /// Attempts to acquire a permit, returning an error if there are none available.
    pub fn try_acquire_owned(&self) -> Result<OwnedSemaphorePermit, NoPermits> {
        let permit = self
            .inner
            .clone()
            .try_acquire_owned()
            .map_err(|_| NoPermits)?;

        Ok(OwnedSemaphorePermit { _permit: permit })
    }
}
