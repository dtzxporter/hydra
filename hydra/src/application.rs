use std::future::Future;
use std::sync::Once;
use std::time::Duration;

use serde::Deserialize;
use serde::Serialize;

use tokio::runtime::Builder;
use tokio::runtime::Runtime;
use tokio::sync::oneshot;

use crate::ExitReason;
use crate::Message;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;

/// Messages used internally by [Application].
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum ApplicationMessage {
    ShutdownTimeout,
}

/// Main application logic and entry point for a hydra program.
///
/// [Application] provides graceful shutdown by allowing you to link a process inside the call to `start`.
/// The `run` call will only return once that process has terminated. It's recommended to link a supervisor.
pub trait Application: Sized + Send + 'static {
    /// Whether or not tracing will subscribe when you call `run`.
    ///
    /// This will install a global tracing subscriber with recommended settings for you.
    #[cfg(feature = "tracing")]
    const TRACING_SUBSCRIBE: bool = true;
    /// Whether or not tracing will be used to catch panics globally.
    ///
    /// This will install a global panic hook that will log panics using `tracing`.
    #[cfg(feature = "tracing")]
    const TRACING_PANICS: bool = true;

    /// Whether or not to listen for shutdown signals and allow the program to cleanup processes before closing.
    const GRACEFUL_SHUTDOWN: bool = true;

    /// The maximum amount of time to wait for the application to shutdown before exiting.
    const GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(10);

    /// Called when an application is starting. You should link a process here and return it's [Pid].
    ///
    /// The [Application] will wait for that process to exit before returning from `run`.
    fn start(&self) -> impl Future<Output = Result<Pid, ExitReason>> + Send;

    /// Runs the [Application] to completion.
    ///
    /// This method will return when the linked process created in `start` has exited.
    fn run(self) {
        use ApplicationMessage::*;

        if Self::TRACING_SUBSCRIBE {
            static TRACING_SUBSCRIBE_ONCE: Once = Once::new();

            TRACING_SUBSCRIBE_ONCE.call_once(|| {
                tracing_subscriber::fmt::init();
            });
        }

        let mut prev_hook: Option<_> = None;

        if Self::TRACING_PANICS {
            prev_hook = Some(std::panic::take_hook());

            std::panic::set_hook(Box::new(tracing_panic::panic_hook));
        }

        let rt = Runtime::new().unwrap();

        rt.block_on(async move {
            let (tx, rx) = oneshot::channel();

            Process::spawn(async move {
                Process::set_flags(ProcessFlags::TRAP_EXIT);

                match self.start().await {
                    Ok(pid) => {
                        #[cfg(feature = "tracing")]
                        tracing::info!(supervisor = ?pid, "Application supervisor has started");

                        let spid = if Self::GRACEFUL_SHUTDOWN {
                            Some(Process::spawn_link(signal_handler()))
                        } else {
                            None
                        };

                        loop {
                            let message = Process::receive::<ApplicationMessage>().await;

                            match message {
                                Message::User(ShutdownTimeout) => {
                                    #[cfg(feature = "tracing")]
                                    tracing::error!(timeout = ?Self::GRACEFUL_SHUTDOWN_TIMEOUT, "Application failed to shutdown gracefully");

                                    Process::exit(pid, ExitReason::Kill);
                                }
                                Message::System(SystemMessage::Exit(epid, ereason)) => {
                                    if epid == pid {
                                        if ereason.is_custom() && ereason != "shutdown" {
                                            #[cfg(feature = "tracing")]
                                            tracing::error!(reason = ?ereason, supervisor = ?epid, "Application supervisor has terminated");
                                        } else {
                                            #[cfg(feature = "tracing")]
                                            tracing::info!(reason = ?ereason, supervisor = ?epid, "Application supervisor has exited");
                                        }
                                        break;
                                    } else if spid.is_some_and(|spid| spid == epid) {
                                        #[cfg(feature = "tracing")]
                                        tracing::info!(reason = ?ereason, supervisor = ?epid, timeout = ?Self::GRACEFUL_SHUTDOWN_TIMEOUT, "Application starting graceful shutdown");

                                        Process::exit(pid, ExitReason::from("shutdown"));
                                        Process::send_after(Process::current(), ShutdownTimeout, Self::GRACEFUL_SHUTDOWN_TIMEOUT);
                                    }
                                }
                                _ => continue,
                            }
                        }
                    }
                    Err(reason) => {
                        #[cfg(feature = "tracing")]
                        tracing::error!(reason = ?reason, "Application supervisor failed to start");

                        #[cfg(not(feature = "tracing"))]
                        let _ = reason;
                    }
                }

                tx.send(()).unwrap();
            });

            let _ = rx.await;
        });

        if let Some(prev_hook) = prev_hook {
            std::panic::set_hook(prev_hook);
        }
    }

    /// Runs the [Application] to completion for tests.
    ///
    /// This method will panic if the process doesn't cleanly exit with `normal` or `shutdown` reasons.
    fn test(self) {
        let rt = Builder::new_current_thread().enable_all().build().unwrap();

        rt.block_on(async move {
            let (tx, rx) = oneshot::channel();

            Process::spawn(async move {
                Process::set_flags(ProcessFlags::TRAP_EXIT);

                match self.start().await {
                    Ok(pid) => loop {
                        let message = Process::receive::<()>().await;

                        match message {
                            Message::System(SystemMessage::Exit(epid, ereason)) => {
                                if epid == pid {
                                    tx.send(Some(ereason)).unwrap();
                                    break;
                                }
                            }
                            _ => continue,
                        }
                    },
                    Err(reason) => {
                        tx.send(Some(reason)).unwrap();
                    }
                }
            });

            if let Ok(Some(reason)) = rx.await {
                if reason.is_custom() && reason != "shutdown" {
                    panic!("Exited: {:?}", reason);
                }
            }
        });
    }
}

/// Handles SIGTERM and ctrl+c signals on unix-like platforms.
#[cfg(unix)]
async fn signal_handler() {
    use tokio::signal::unix;

    let mut sigterm =
        unix::signal(unix::SignalKind::terminate()).expect("Failed to register SIGTERM handler");

    tokio::select! {
        _ = sigterm.recv() => {
            Process::exit(Process::current(), ExitReason::from("SIGTERM"));
        }
        _ = tokio::signal::ctrl_c() => {
            Process::exit(Process::current(), ExitReason::from("CTRL_C"));
        }
    }
}

/// Handles ctrl+c signals on non-unix-like platforms.
#[cfg(not(unix))]
async fn signal_handler() {
    let _ = tokio::signal::ctrl_c().await;

    Process::exit(Process::current(), ExitReason::from("CTRL_C"));
}
