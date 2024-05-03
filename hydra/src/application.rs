use std::future::Future;

use tokio::runtime::Builder;
use tokio::runtime::Runtime;
use tokio::sync::oneshot;

use crate::ExitReason;
use crate::Message;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;

/// Main application logic and entry point for a hydra program.
///
/// [Application] provides graceful shutdown by allowing you to link a process inside the call to `start`.
/// The `run` call will only return once that process has terminated. It's recommended to link a supervisor.
pub trait Application: Sized + Send + 'static {
    /// Whether or not tracing will be used to catch panics globally.
    ///
    /// This will install a global panic hook that will log panics using `tracing`.
    #[cfg(feature = "tracing")]
    const TRACING_PANICS: bool = false;

    /// Called when an application is starting. You should link a process here and return it's [Pid].
    ///
    /// The [Application] will wait for that process to exit before returning from `run`.
    fn start(&self) -> impl Future<Output = Result<Pid, ExitReason>> + Send;

    /// Runs the [Application] to completion.
    ///
    /// This method will return when the linked process created in `start` has exited.
    fn run(self) {
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
                        tracing::info!(supervisor = ?pid, "Application supervisor has started.");

                        loop {
                            let message = Process::receive::<()>().await;

                            match message {
                                Message::System(SystemMessage::Exit(epid, ereason)) => {
                                    if epid == pid {
                                        if ereason.is_custom() && ereason != "shutdown" {
                                            #[cfg(feature = "tracing")]
                                            tracing::error!(reason = ?ereason, supervisor = ?epid, "Application supervisor has terminated.");
                                        } else {
                                            #[cfg(feature = "tracing")]
                                            tracing::info!(reason = ?ereason, supervisor = ?epid, "Application supervisor has exited.");
                                        }
                                        break;
                                    }
                                }
                                _ => continue,
                            }
                        }
                    }
                    Err(reason) => {
                        #[cfg(feature = "tracing")]
                        tracing::error!(reason = ?reason, "Application supervisor failed to start.");

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
