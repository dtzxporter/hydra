use std::future::Future;

use serde::Deserialize;
use serde::Serialize;

use tokio::runtime::Builder;
use tokio::runtime::Runtime;
use tokio::sync::oneshot;

use crate::ApplicationConfig;
use crate::ExitReason;
use crate::Message;
use crate::Pid;
use crate::Process;
use crate::ProcessFlags;
use crate::SystemMessage;

#[cfg(feature = "console")]
use crate::ConsoleServer;

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
    /// Override to change the application configuration defaults.
    fn config() -> ApplicationConfig {
        ApplicationConfig::default()
    }

    /// Called when an application is starting. You should link a process here and return it's [Pid].
    ///
    /// The [Application] will wait for that process to exit before returning from `run`.
    fn start(&self) -> impl Future<Output = Result<Pid, ExitReason>> + Send;

    /// Runs the [Application] to completion.
    ///
    /// This method will return when the linked process created in `start` has exited.
    fn run(self) {
        use ApplicationMessage::*;

        let config = Self::config();

        #[cfg(feature = "tracing")]
        if config.tracing_subscribe {
            use std::sync::Once;

            static TRACING_SUBSCRIBE_ONCE: Once = Once::new();

            TRACING_SUBSCRIBE_ONCE.call_once(|| {
                tracing_subscriber::fmt::init();
            });
        }

        #[allow(unused_mut)]
        let mut prev_hook: Option<_> = None;

        #[cfg(feature = "tracing")]
        if config.tracing_panics {
            prev_hook = Some(std::panic::take_hook());

            std::panic::set_hook(Box::new(panic_hook));
        }

        let rt = Runtime::new().unwrap();

        rt.block_on(async move {
            let (tx, rx) = oneshot::channel();

            Process::spawn(async move {
                Process::set_flags(ProcessFlags::TRAP_EXIT);

                #[cfg(feature="console")]
                let mut cpid = ConsoleServer::new()
                                        .start_link()
                                        .await
                                        .expect("Failed to start console server!");

                match self.start().await {
                    Ok(pid) => {
                        #[cfg(feature = "tracing")]
                        tracing::info!(supervisor = ?pid, "Application supervisor has started");

                        let spid = if config.graceful_shutdown {
                            Some(Process::spawn_link(signal_handler()))
                        } else {
                            None
                        };

                        loop {
                            let message = Process::receive::<ApplicationMessage>().await;

                            match message {
                                Message::User(ShutdownTimeout) => {
                                    #[cfg(feature = "tracing")]
                                    tracing::error!(timeout = ?config.graceful_shutdown_timeout, "Application failed to shutdown gracefully");

                                    Process::exit(pid, ExitReason::Kill);
                                }
                                Message::System(SystemMessage::Exit(epid, ereason)) => {
                                    if epid == pid {
                                        if ereason.is_custom() && ereason != "shutdown" {
                                            #[cfg(feature = "tracing")]
                                            tracing::error!(reason = ?ereason, supervisor = ?pid, "Application supervisor has terminated");
                                        } else {
                                            #[cfg(feature = "tracing")]
                                            tracing::info!(reason = ?ereason, supervisor = ?pid, "Application supervisor has exited");
                                        }
                                        break;
                                    } else if spid.is_some_and(|spid| spid == epid) {
                                        #[cfg(feature = "tracing")]
                                        tracing::info!(reason = ?ereason, supervisor = ?pid, timeout = ?config.graceful_shutdown_timeout, "Application starting graceful shutdown");

                                        Process::exit(pid, ExitReason::from("shutdown"));
                                        Process::send_after(Process::current(), ShutdownTimeout, config.graceful_shutdown_timeout);
                                    }

                                    #[cfg(feature = "console")]
                                    if cpid == epid && ereason != "shutdown" {
                                        cpid = ConsoleServer::new()
                                                        .start_link()
                                                        .await
                                                        .expect("Failed to restart console server!");
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
            Process::exit(Process::current(), ExitReason::from("sigterm"));
        }
        _ = tokio::signal::ctrl_c() => {
            Process::exit(Process::current(), ExitReason::from("ctrl_c"));
        }
    }
}

/// Handles ctrl+c signals on non-unix-like platforms.
#[cfg(not(unix))]
async fn signal_handler() {
    let _ = tokio::signal::ctrl_c().await;

    Process::exit(Process::current(), ExitReason::from("ctrl_c"));
}

/// Handles forwarding panic messages through tracing when enabled.
#[cfg(feature = "tracing")]
fn panic_hook(panic_info: &std::panic::PanicHookInfo) {
    use std::backtrace::Backtrace;
    use std::backtrace::BacktraceStatus;

    use tracing::*;

    let payload = panic_info.payload();

    #[allow(clippy::manual_map)]
    let payload = if let Some(s) = payload.downcast_ref::<&str>() {
        Some(&**s)
    } else if let Some(s) = payload.downcast_ref::<String>() {
        Some(s.as_str())
    } else {
        None
    };

    let location = panic_info.location().map(|location| location.to_string());

    let backtrace = Backtrace::capture();
    let backtrace = if backtrace.status() == BacktraceStatus::Disabled {
        String::from("run with RUST_BACKTRACE=1 environment variable to display a backtrace")
    } else {
        field::display(backtrace).to_string()
    };

    event!(
        target: "hydra",
        Level::ERROR,
        payload = payload,
        location = location,
        backtrace = ?backtrace,
        "A process has panicked",
    );
}
