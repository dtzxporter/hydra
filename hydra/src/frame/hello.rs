use std::net::SocketAddr;

use sha2::Sha256;

use hmac::Hmac;
use hmac::Mac;

use bincode::Decode;
use bincode::Encode;

use crate::NODE_COOKIE;

/// Hmac using sha256.
type HmacSha256 = Hmac<Sha256>;

#[derive(Debug, Encode, Decode)]
pub struct Hello {
    pub name: String,
    pub broadcast_address: SocketAddr,
    pub challenge: Vec<u8>,
}

impl Hello {
    /// Constructs a new instance of the [Hello] frame.
    pub fn new(name: String, broadcast_address: SocketAddr) -> Self {
        let mut challenge = {
            let cookie = NODE_COOKIE.lock().unwrap();
            let cookie = cookie
                .as_ref()
                .map(|cookie| cookie.as_bytes())
                .unwrap_or(&[0]);

            HmacSha256::new_from_slice(cookie).unwrap()
        };

        challenge.update(name.as_bytes());
        challenge.update(broadcast_address.to_string().as_bytes());

        Self {
            name,
            broadcast_address,
            challenge: challenge.finalize().into_bytes().to_vec(),
        }
    }

    /// Validates this [Hello] frame against our cookie.
    pub fn validate(&self) -> bool {
        let mut challenge = {
            let cookie = NODE_COOKIE.lock().unwrap();
            let cookie = cookie
                .as_ref()
                .map(|cookie| cookie.as_bytes())
                .unwrap_or(&[0]);

            HmacSha256::new_from_slice(cookie).unwrap()
        };

        challenge.update(self.name.as_bytes());
        challenge.update(self.broadcast_address.to_string().as_bytes());

        let wanted = challenge.finalize().into_bytes();

        wanted.as_slice() == self.challenge
    }
}
