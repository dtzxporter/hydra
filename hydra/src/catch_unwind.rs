use std::future::Future;
use std::panic::catch_unwind;
use std::panic::AssertUnwindSafe;
use std::panic::UnwindSafe;

use pin_project_lite::pin_project;

pin_project! {
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
    pub fn new(future: Fut) -> Self {
        Self { future }
    }
}

impl<Fut> Future for CatchUnwind<Fut>
where
    Fut: Future + UnwindSafe,
{
    type Output = Result<Fut::Output, String>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
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
