use crate::ExitReason;
use crate::Pid;

pub enum SystemMessage {
    Exit(Pid, ExitReason),
}
