use bincode::Decode;
use bincode::Encode;

/// The frame used to keep the connection alive.
#[derive(Debug, Encode, Decode)]
pub struct Pong;
