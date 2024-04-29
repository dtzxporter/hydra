use std::io::Error;
use std::io::ErrorKind;

use bincode::Decode;
use bincode::Encode;

use bincode::config;
use bincode::config::Configuration;
use bincode::config::Fixint;
use bincode::config::LittleEndian;
use bincode::error::DecodeError;

use bytes::Buf;
use bytes::BufMut;
use bytes::BytesMut;

use tokio_util::codec::Decoder;
use tokio_util::codec::Encoder;

mod exit;
mod hello;
mod link;
mod link_down;
mod monitor;
mod monitor_down;
mod monitor_update;
mod ping;
mod pong;
mod send;

pub use exit::*;
pub use hello::*;
pub use link::*;
pub use link_down::*;
pub use monitor::*;
pub use monitor_down::*;
pub use monitor_update::*;
pub use ping::*;
pub use pong::*;
pub use send::*;

/// Bincode configuration for the frame codec.
const FRAME_CONFIG: Configuration<LittleEndian, Fixint> = config::standard()
    .with_fixed_int_encoding()
    .with_little_endian();

/// A frame value for the codec.
#[derive(Debug, Encode, Decode)]
pub enum Frame {
    Hello(Hello),
    Ping,
    Pong,
    Send(Send),
    Monitor(Monitor),
    MonitorDown(MonitorDown),
    MonitorUpdate(MonitorUpdate),
    Link(Link),
    LinkDown(LinkDown),
    Exit(Exit),
}

impl From<Hello> for Frame {
    fn from(value: Hello) -> Self {
        Self::Hello(value)
    }
}

impl From<Ping> for Frame {
    fn from(_: Ping) -> Self {
        Self::Ping
    }
}

impl From<Pong> for Frame {
    fn from(_: Pong) -> Self {
        Self::Pong
    }
}

impl From<Send> for Frame {
    fn from(value: Send) -> Self {
        Self::Send(value)
    }
}

impl From<Monitor> for Frame {
    fn from(value: Monitor) -> Self {
        Self::Monitor(value)
    }
}

impl From<MonitorDown> for Frame {
    fn from(value: MonitorDown) -> Self {
        Self::MonitorDown(value)
    }
}

impl From<MonitorUpdate> for Frame {
    fn from(value: MonitorUpdate) -> Self {
        Self::MonitorUpdate(value)
    }
}

impl From<Link> for Frame {
    fn from(value: Link) -> Self {
        Self::Link(value)
    }
}

impl From<LinkDown> for Frame {
    fn from(value: LinkDown) -> Self {
        Self::LinkDown(value)
    }
}

impl From<Exit> for Frame {
    fn from(value: Exit) -> Self {
        Self::Exit(value)
    }
}

/// The frame codec.
#[derive(Default)]
pub struct Codec;

impl Codec {
    /// Constructs a new instance of [Codec] with default settings.
    pub const fn new() -> Self {
        Self
    }
}

impl Encoder<Frame> for Codec {
    type Error = Error;

    fn encode(&mut self, item: Frame, dst: &mut BytesMut) -> Result<(), Self::Error> {
        bincode::encode_into_std_write(item, &mut dst.writer(), FRAME_CONFIG)
            .map_err(|e| Error::new(ErrorKind::InvalidInput, e))?;
        Ok(())
    }
}

impl Decoder for Codec {
    type Item = Frame;
    type Error = Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if src.is_empty() {
            return Ok(None);
        }

        let result = bincode::decode_from_slice(src, FRAME_CONFIG);

        match result {
            Ok((frame, length)) => {
                src.advance(length);
                Ok(Some(frame))
            }
            Err(DecodeError::UnexpectedEnd { additional }) => {
                src.reserve(additional);
                Ok(None)
            }
            Err(e) => Err(Error::new(ErrorKind::InvalidData, e)),
        }
    }
}
