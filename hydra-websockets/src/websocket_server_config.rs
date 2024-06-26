use std::net::SocketAddr;
use std::time::Duration;

/// A set of configuration options for the websocket server.
#[derive(Debug, Clone)]
pub struct WebsocketServerConfig {
    pub(crate) address: SocketAddr,
    pub(crate) handshake_timeout: Option<Duration>,
    pub(crate) max_message_size: Option<usize>,
    pub(crate) max_frame_size: Option<usize>,
    #[cfg(feature = "native-tls")]
    pub(crate) tls_pkcs12_der: Option<Vec<u8>>,
    #[cfg(feature = "native-tls")]
    pub(crate) tls_pkcs12_password: Option<String>,
    #[cfg(feature = "native-tls")]
    pub(crate) tls_pkcs8_pem: Option<Vec<u8>>,
    #[cfg(feature = "native-tls")]
    pub(crate) tls_pkcs8_key: Option<Vec<u8>>,
}

impl WebsocketServerConfig {
    /// Constructs a new [WebsocketServerConfig] with the given address to listen on.
    pub fn new(address: SocketAddr) -> Self {
        use tokio_tungstenite::tungstenite;
        use tungstenite::protocol::*;

        let defaults = WebSocketConfig::default();

        Self {
            address,
            handshake_timeout: None,
            max_message_size: defaults.max_message_size,
            max_frame_size: defaults.max_frame_size,
            #[cfg(feature = "native-tls")]
            tls_pkcs12_der: None,
            #[cfg(feature = "native-tls")]
            tls_pkcs12_password: None,
            #[cfg(feature = "native-tls")]
            tls_pkcs8_pem: None,
            #[cfg(feature = "native-tls")]
            tls_pkcs8_key: None,
        }
    }

    /// Sets the timeout for websocket handshakes.
    ///
    /// When tls is enabled, this time includes the tls handshake as well.
    pub const fn handshake_timeout(mut self, timeout: Duration) -> Self {
        self.handshake_timeout = Some(timeout);
        self
    }

    /// Sets the maximum size of an incoming message. `None` means no size limit. The default value is 64 MiB
    /// which should be reasonably big for all normal use-cases but small enough to prevent
    /// memory eating by a malicious user.
    pub const fn max_message_size(mut self, size: Option<usize>) -> Self {
        self.max_message_size = size;
        self
    }

    /// Sets the maximum size of a single incoming message frame. `None` means no size limit. The limit is for
    /// frame payload NOT including the frame header. The default value is 16 MiB which should
    /// be reasonably big for all normal use-cases but small enough to prevent memory eating
    /// by a malicious user.
    pub const fn max_frame_size(mut self, size: Option<usize>) -> Self {
        self.max_frame_size = size;
        self
    }

    /// Loads a DER-formatted PKCS #12 archive, using the specified `password` to decrypt the key.
    ///
    /// The archive should contain a leaf certificate and its private key,
    /// as well any intermediate certificates that should be sent to clients to allow them to build a chain to a trusted root.
    /// The chain certificates should be in order from the leaf certificate towards the root.
    ///
    /// PKCS #12 archives typically have the file extension .p12 or .pfx, and can be created with the OpenSSL pkcs12 tool:
    /// ```ignore
    /// openssl pkcs12 -export -out identity.pfx -inkey key.pem -in cert.pem -certfile chain_certs.pem
    /// ```
    #[cfg(feature = "native-tls")]
    pub fn tls_pkcs12<D: Into<Vec<u8>>, S: Into<String>>(mut self, der: D, password: S) -> Self {
        self.tls_pkcs12_der = Some(der.into());
        self.tls_pkcs12_password = Some(password.into());
        self
    }

    /// Loads a chain of PEM encoded X509 certificates, with the leaf certificate first.
    /// `key` is a PEM encoded PKCS #8 formatted private key for the leaf certificate.
    ///
    /// The certificate chain should contain any intermediate cerficates that should be sent to clients to allow them to build a chain to a trusted root.
    ///
    /// A certificate chain here means a series of PEM encoded certificates concatenated together.
    #[cfg(feature = "native-tls")]
    pub fn tls_pkcs8<P: Into<Vec<u8>>, K: Into<Vec<u8>>>(mut self, pem: P, key: K) -> Self {
        self.tls_pkcs8_pem = Some(pem.into());
        self.tls_pkcs8_key = Some(key.into());
        self
    }
}
