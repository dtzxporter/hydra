use bitflags::bitflags;

bitflags! {
    /// A collection of configurable flags for a process.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct ProcessFlags : u32 {
        /// Whether or not the process is trapping exits.
        const TRAP_EXIT = 1 << 0;
    }
}
