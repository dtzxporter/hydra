use std::future::Future;
use std::panic::catch_unwind;
use std::panic::AssertUnwindSafe;
use std::panic::UnwindSafe;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use pin_project_lite::pin_project;

pin_project! {
    /// A future that will catch panics and unwind them.
    pub struct CatchUnwind<Fut>
    where
        Fut: Future,
    {
        #[pin]
        future: Fut,
    }
}

impl<Fut> CatchUnwind<Fut>
where
    Fut: Future + UnwindSafe,
{
    /// Constructs a new [CatchUnwind] for the given future.
    pub fn new(future: Fut) -> Self {
        Self { future }
    }
}

impl<Fut> Future for CatchUnwind<Fut>
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
