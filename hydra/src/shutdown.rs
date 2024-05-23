use std::time::Duration;

use crate::ExitReason;
use crate::Message;
use crate::Pid;
use crate::Process;
use crate::Reference;
use crate::SystemMessage;

/// Defines how a child process should be terminated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Shutdown {
    /// The child process is unconditionally and immediately terminated using `Process::exit(child, ExitReason::Kill)`.
    BrutalKill,
    /// The amount of time that the parent will wait for it's children to terminate after emitting a
    /// `Process::exit(child, ExitReason::from("shutdown"))` signal. If the child process is not trapping exits, the initial `shutdown` signal
    /// will terminate the child process immediately. If the child process is trapping exits, it has the given duration to terminate.
    Duration(Duration),
    /// The parent will wait indefinitely for the child to terminate.
    Infinity,
}

/// Defines how a superviser should handle shutdown when a significant process exits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AutoShutdown {
    /// This is the default, automatic shutdown is disabled.
    Never,
    /// If any significant child process exits, the supervisor will automatically shut down it's children, then itself.
    AnySignificant,
    /// When all significant child processes have exited, the supervisor will automatically shut down it's children, then itself.
    AllSignificant,
}

impl From<Duration> for Shutdown {
    fn from(value: Duration) -> Self {
        Self::Duration(value)
    }
}

/// Terminates the given `pid` by forcefully killing it and waiting for the `monitor` to fire.
pub(crate) async fn shutdown_brutal_kill(pid: Pid, monitor: Reference) -> Result<(), ExitReason> {
    Process::exit(pid, ExitReason::Kill);

    let result = Process::receiver()
        .for_message::<()>()
        .select(|message| {
            match message {
                Message::System(SystemMessage::ProcessDown(_, tag, _)) => {
                    // Make sure that the tag matches.
                    *tag == monitor
                }
                _ => false,
            }
        })
        .await;

    let Message::System(SystemMessage::ProcessDown(_, _, reason)) = result else {
        unreachable!()
    };

    unlink_flush(pid, reason);

    Ok(())
}

/// Terminates the given `pid` by gracefully waiting for `timeout`
/// then forcefully kills it as necessary while waiting for `monitor` to fire.
pub(crate) async fn shutdown_timeout(
    pid: Pid,
    monitor: Reference,
    timeout: Duration,
) -> Result<(), ExitReason> {
    Process::exit(pid, ExitReason::from("shutdown"));

    let receiver = Process::receiver()
                        .select(|message| matches!(message, Message::System(SystemMessage::ProcessDown(_, tag, _)) if *tag == monitor));

    let result = Process::timeout(timeout, receiver).await;

    match result {
        Ok(Message::System(SystemMessage::ProcessDown(_, _, reason))) => {
            unlink_flush(pid, reason);

            Ok(())
        }
        Ok(_) => unreachable!(),
        Err(_) => shutdown_brutal_kill(pid, monitor).await,
    }
}

/// Terminates the given `pid` by gracefully waiting indefinitely for the `monitor` to fire.
pub(crate) async fn shutdown_infinity(pid: Pid, monitor: Reference) -> Result<(), ExitReason> {
    Process::exit(pid, ExitReason::from("shutdown"));

    let result = Process::receiver()
                    .select(|message| matches!(message, Message::System(SystemMessage::ProcessDown(_, tag, _)) if *tag == monitor))
                    .await;

    let Message::System(SystemMessage::ProcessDown(_, _, reason)) = result else {
        unreachable!()
    };

    unlink_flush(pid, reason);

    Ok(())
}

/// Unlinks the given process and ensures that any pending exit signal is flushed from the message queue.
///
/// Returns the real [ExitReason] or the `default_reason` if no signal was found.
fn unlink_flush(pid: Pid, default_reason: ExitReason) -> ExitReason {
    Process::unlink(pid);

    let mut reason = default_reason;

    Process::receiver().remove(|message| match message {
        Message::System(SystemMessage::Exit(epid, ereason)) => {
            if *epid == pid {
                reason = ereason.clone();
                return true;
            }

            false
        }
        _ => false,
    });

    reason
}
