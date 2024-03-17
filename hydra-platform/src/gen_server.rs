use std::future::Future;

use hydra::Pid;
use hydra::Process;

use tokio::sync::oneshot;
use tokio::time::timeout;

use crate::GenServerOptions;

pub trait GenServer: Sized + Send + 'static {
    /// The type of the init argument that this server will use.
    type InitArg: Send;
    /// The message type that this server will use.
    type Message: Send + 'static;

    fn init(&mut self, init_arg: Self::InitArg) -> impl Future<Output = ()> + Send;

    async fn start_link(self, init_arg: Self::InitArg, options: GenServerOptions) -> Pid {
        start_internal(self, init_arg, options, true).await
    }

    /// A method.
    #[allow(unused_variables)]
    async fn handle_cast(&mut self, from: Pid, message: Self::Message) {}
}

async fn start_internal<T: GenServer + Send + 'static>(
    gen_server: T,
    init_arg: T::InitArg,
    mut options: GenServerOptions,
    link: bool,
) -> Pid {
    let (tx, rx) = oneshot::channel::<()>();

    let server = async move {
        let mut gen_server = gen_server;

        if let Some(duration) = options.timeout.take() {
            let _ = timeout(duration, gen_server.init(init_arg)).await;
        } else {
            gen_server.init(init_arg).await;
        }

        let _ = tx.send(());
    };

    let pid = if link {
        Process::spawn_link(server)
    } else {
        Process::spawn(server)
    };

    let _ = rx.await;

    pid
}
