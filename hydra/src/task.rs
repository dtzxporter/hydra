use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures_util::FutureExt;

use tokio::task::JoinHandle;

/// A task is a lightweight thread of execution designed to run one particular action.
///
/// Tasks can not receive messages from other processes, only send.
///
/// Calls that are allowed:
/// - Process::send
/// - GenServer::cast
///
/// Calls that would panic:
/// - Process::link
/// - Process::monitor
/// - GenServer::call
///
/// You can however return a value in a task and await it to receive the value.
///
/// It's not recommended to `await` long running tasks in a `GenServer` since it will delay processing of other messages.
/// Instead, you should send a `cast` with the result of your task and handle it in `handle_cast`.
pub struct Task;

/// The result of a spawned task, can be used to await the task result, or shutdown the task.
#[repr(transparent)]
pub struct TaskHandle<R> {
    handle: JoinHandle<R>,
}

/// An error occured while executing the task.
#[derive(Debug)]
pub struct TaskError(String);

impl Task {
    /// Runs the provided asynchronous `task`.
    pub fn spawn<F>(task: F) -> TaskHandle<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        TaskHandle {
            handle: tokio::task::spawn(task),
        }
    }

    /// Runs the provided synchronous `task` on a thread where blocking is acceptable.
    pub fn spawn_blocking<F, R>(task: F) -> TaskHandle<R>
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send + 'static,
    {
        TaskHandle {
            handle: tokio::task::spawn_blocking(task),
        }
    }

    /// Shuts down the task, and then checks for a result.
    ///
    /// Returns the result if the task finishes while shutting down, [TaskError] if the task died before returning.
    pub async fn shutdown<R>(task: TaskHandle<R>) -> Result<R, TaskError> {
        task.handle.abort();
        task.handle
            .await
            .map_err(|error| TaskError(error.to_string()))
    }

    /// Await many tasks at once and returns their results in order, or returns the first error that occurs.
    pub async fn await_many<R, const N: usize>(
        tasks: [TaskHandle<R>; N],
    ) -> Result<Vec<R>, TaskError>
    where
        R: 'static,
    {
        let mut results: Vec<R> = Vec::with_capacity(N);

        for task in tasks {
            results.push(
                task.handle
                    .await
                    .map_err(|error| TaskError(error.to_string()))?,
            );
        }

        Ok(results)
    }
}

impl<R> Future for TaskHandle<R>
where
    R: Send + 'static,
{
    type Output = Result<R, TaskError>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.handle
            .poll_unpin(cx)
            .map_err(|error| TaskError(error.to_string()))
    }
}
