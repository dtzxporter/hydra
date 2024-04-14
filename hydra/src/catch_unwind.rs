use std::future::Future;
use std::panic::catch_unwind;
use std::panic::AssertUnwindSafe;
use std::panic::UnwindSafe;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use pin_project_lite::pin_project;

/// A function that will catch panics and unwind them.
pub struct CatchUnwind<Fun, R>
where
    Fun: FnOnce() -> R + Send + UnwindSafe + 'static,
    R: Send + 'static,
{
    function: Fun,
}

impl<Fun, R> CatchUnwind<Fun, R>
where
    Fun: FnOnce() -> R + Send + UnwindSafe + 'static,
    R: Send + 'static,
{
    /// Constructs a new instance of [CatchUnwind].
    #[allow(clippy::new_ret_no_self)]
    pub fn new(function: Fun) -> Result<R, String> {
        Self { function }.catch_unwind()
    }

    /// Executes the function returning any error that occured.
    fn catch_unwind(self) -> Result<R, String> {
        catch_unwind(self.function).map_err(|x| {
            if x.is::<String>() {
                return *x.downcast::<String>().unwrap();
            } else if x.is::<&str>() {
                return x.downcast::<&str>().unwrap().to_string();
            }

            "Unknown error!".to_string()
        })
    }
}

pin_project! {
    /// A future that will catch panics and unwind them.
    pub struct AsyncCatchUnwind<Fut>
    where
        Fut: Future,
    {
        #[pin]
        future: Fut,
    }
}

impl<Fut> AsyncCatchUnwind<Fut>
where
    Fut: Future + UnwindSafe,
{
    /// Constructs a new [CatchUnwind] for the given future.
    pub fn new(future: Fut) -> Self {
        Self { future }
    }
}

impl<Fut> Future for AsyncCatchUnwind<Fut>
where
    Fut: Future + UnwindSafe,
{
    type Output = Result<Fut::Output, String>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let f = self.project().future;

        catch_unwind(AssertUnwindSafe(|| f.poll(cx)))
            .map_err(|x| {
                if x.is::<String>() {
                    return *x.downcast::<String>().unwrap();
                } else if x.is::<&str>() {
                    return x.downcast::<&str>().unwrap().to_string();
                }

                "Unknown error!".to_string()
            })?
            .map(Ok)
    }
}
