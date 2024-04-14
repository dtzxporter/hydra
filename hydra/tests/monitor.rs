use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

use hydra::ExitReason;
use hydra::Message;
use hydra::Process;
use hydra::SystemMessage;

#[hydra::test]
async fn monitor_works() {
    let pid = Process::spawn(async {
        tokio::time::sleep(Duration::from_millis(50)).await;
        panic!("we're going down!");
    });

    let reference = Process::monitor(pid);

    let message: Message<()> = Process::receive().await;

    if let Message::System(SystemMessage::ProcessDown(object, mref, exit_reason)) = message {
        assert!(object == pid);
        assert!(reference == mref);
        assert!(matches!(exit_reason, ExitReason::Custom(_)));
    } else {
        panic!("Expected process down message!");
    }
}

#[hydra::test]
async fn monitor_named_works() {
    let pid = Process::spawn(async {
        tokio::time::sleep(Duration::from_millis(50)).await;
        panic!("we're going down!");
    });

    Process::register(pid, "monitor_me");

    let reference = Process::monitor("monitor_me");

    let message: Message<()> = Process::receive().await;

    if let Message::System(SystemMessage::ProcessDown(object, mref, exit_reason)) = message {
        assert!(object == "monitor_me");
        assert!(reference == mref);
        assert!(matches!(exit_reason, ExitReason::Custom(_)));
    } else {
        panic!("Expected process down message!");
    }
}

#[hydra::test]
async fn demonitor_works() {
    let is_down: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));
    let is_down_ref = is_down.clone();

    let (_, monitor) = Process::spawn_monitor(async move {
        tokio::time::sleep(Duration::from_millis(50)).await;
        is_down_ref.store(true, Ordering::Relaxed);
        panic!("we're going down!");
    });

    Process::demonitor(monitor);

    tokio::time::sleep(Duration::from_millis(75)).await;

    assert!(is_down.load(Ordering::Relaxed));

    Process::send(Process::current(), ());

    let message: Message<()> = Process::receive().await;

    assert!(matches!(message, Message::User(())));
}

#[hydra::test]
async fn spawn_monitor_works() {
    let (pid, reference) = Process::spawn_monitor(async {
        panic!("we're going down!");
    });

    let message: Message<()> = Process::receive().await;

    if let Message::System(SystemMessage::ProcessDown(from, mref, exit_reason)) = message {
        assert!(from == pid);
        assert!(reference == mref);
        assert!(matches!(exit_reason, ExitReason::Custom(_)));
    } else {
        panic!("Expected process down message!");
    }
}
