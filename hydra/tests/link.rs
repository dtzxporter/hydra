use std::time::Duration;

use hydra::Message;
use hydra::Process;
use hydra::ProcessFlags;
use hydra::SystemMessage;

#[hydra::test]
async fn link_works() {
    let pid = Process::spawn(async {
        Process::spawn_link(async {
            panic!("we died!");
        });
        let _ = Process::receive::<()>().await;
    });

    tokio::time::sleep(Duration::from_millis(10)).await;

    assert!(!Process::alive(pid));
}

#[hydra::test]
async fn link_trap_exit_works() {
    Process::set_flags(ProcessFlags::TRAP_EXIT);

    let pid = Process::spawn_link(async {
        panic!("we died!");
    });

    let message: Message<()> = Process::receive().await;

    if let Message::System(SystemMessage::Exit(from, exit_reason)) = message {
        assert!(from == pid);
        assert!(exit_reason == "we died!");
    } else {
        panic!("Expected exit signal!");
    }
}

#[hydra::test]
async fn unlink_works() {
    let pid = Process::spawn_link(async {
        tokio::time::sleep(Duration::from_millis(10)).await;
        panic!("we died!");
    });

    Process::unlink(pid);
}
