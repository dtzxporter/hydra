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
        Process::sleep(Duration::from_millis(50)).await;
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
        Process::sleep(Duration::from_millis(50)).await;
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
async fn monitor_noproc_works() {
    let pid = Process::spawn(async {
        // End immediately.
    });

    Process::sleep(Duration::from_millis(10)).await;

    let reference1 = Process::monitor("no_exists");
    let reference2 = Process::monitor(pid);

    let message1: Message<()> = Process::receive().await;
    let message2: Message<()> = Process::receive().await;

    if let Message::System(SystemMessage::ProcessDown(object, mref, exit_reason)) = message1 {
        assert!(object == "no_exists");
        assert!(reference1 == mref);
        assert!(matches!(exit_reason, ExitReason::Custom(_)));
    } else {
        panic!("Expected process down message!");
    }

    if let Message::System(SystemMessage::ProcessDown(object, mref, exit_reason)) = message2 {
        assert!(object == pid);
        assert!(reference2 == mref);
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
        Process::sleep(Duration::from_millis(50)).await;
        is_down_ref.store(true, Ordering::Relaxed);
        panic!("we're going down!");
    });

    Process::demonitor(monitor);

    Process::sleep(Duration::from_millis(75)).await;

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

    if let Message::System(SystemMessage::ProcessDown(object, mref, exit_reason)) = message {
        assert!(object == pid);
        assert!(reference == mref);
        assert!(matches!(exit_reason, ExitReason::Custom(_)));
    } else {
        panic!("Expected process down message!");
    }
}
