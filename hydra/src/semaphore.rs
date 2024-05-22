use tokio::sync::Semaphore as SemaphoreBase;
use tokio::sync::SemaphorePermit as SemaphoreBasePermit;

/// A semaphore maintains a set of permits. Permits are used to synchronize access to a shared resource.
/// A semaphore differs from a mutex in that it can allow more than one concurrent caller to access the shared resource at a time.
pub struct Semaphore {
    semaphore: SemaphoreBase,
}

/// Represents a permit to a [Semaphore].
pub struct SemaphorePermit<'a> {
    _permit: SemaphoreBasePermit<'a>,
}

/// Error when there are no permits available.
#[derive(Debug)]
pub struct NoPermits;

impl Semaphore {
    /// Creates a new instance of [Semaphore] with the given `permits` count.
    pub const fn new(permits: usize) -> Self {
        Self {
            semaphore: SemaphoreBase::const_new(permits),
        }
    }

    /// Acquires one permit asynchronously waiting for one to become available.
    #[must_use]
    pub async fn acquire(&self) -> SemaphorePermit {
        let permit = self.semaphore.acquire().await.unwrap();

        SemaphorePermit { _permit: permit }
    }

    /// Acquires many permits asynchronously waiting for them to become available.
    #[must_use]
    pub async fn acquire_many(&self, count: u32) -> SemaphorePermit {
        let permit = self.semaphore.acquire_many(count).await.unwrap();

        SemaphorePermit { _permit: permit }
    }

    /// Attempts to acquire a permit, returning an error if there are none available.
    pub fn try_acquire(&self) -> Result<SemaphorePermit, NoPermits> {
        let permit = self.semaphore.try_acquire().map_err(|_| NoPermits)?;

        Ok(SemaphorePermit { _permit: permit })
    }
}
