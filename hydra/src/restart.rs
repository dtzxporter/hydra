/// Controls what a supervisor should consider to be a successful termination or not.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Restart {
    /// The child process is always restarted.
    Permanent,
    /// The child process is never restarted, regardless of the supervision strategy: any
    /// termination (even abnormal) is considered successful.
    Temporary,
    /// The child process is restarted if it terminates abnormally, i.e., with an exit reason other than
    /// `normal`, or `shutdown`.
    Transient,
}
