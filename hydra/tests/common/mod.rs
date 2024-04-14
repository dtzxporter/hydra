use std::future::Future;

use hydra::Process;

/// A test harness for the hydra runtime.
pub async fn hydra_test<T>(function: T)
where
    T: Future<Output = ()> + Send + 'static,
    T::Output: Send + 'static,
{
    let (tx, rx) = tokio::sync::oneshot::channel();

    Process::spawn(async move {
        function.await;
        tx.send(()).unwrap();
    });

    rx.await.unwrap();
}
