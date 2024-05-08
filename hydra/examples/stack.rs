use hydra::ExitReason;
use hydra::From;
use hydra::GenServer;
use hydra::GenServerOptions;

use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize)]
enum StackMessage {
    Pop,
    PopResult(String),
    Push(String),
}

struct Stack {
    stack: Vec<String>,
}

impl Stack {
    pub fn with_entries(entries: Vec<&'static str>) -> Self {
        Self {
            stack: Vec::from_iter(entries.into_iter().map(Into::into)),
        }
    }
}

impl GenServer for Stack {
    type Message = StackMessage;

    async fn init(&mut self) -> Result<(), ExitReason> {
        Ok(())
    }

    async fn handle_call(
        &mut self,
        message: Self::Message,
        _from: From,
    ) -> Result<Option<Self::Message>, ExitReason> {
        match message {
            StackMessage::Pop => Ok(Some(StackMessage::PopResult(self.stack.remove(0)))),
            _ => unreachable!(),
        }
    }

    async fn handle_cast(&mut self, message: Self::Message) -> Result<(), ExitReason> {
        match message {
            StackMessage::Push(value) => self.stack.insert(0, value),
            _ => unreachable!(),
        }
        Ok(())
    }
}

#[hydra::main]
async fn main() {
    let pid = Stack::with_entries(vec!["hello", "world"])
        .start_link(GenServerOptions::new())
        .await
        .expect("Failed to start stack!");

    let result = Stack::call(pid, StackMessage::Pop, None)
        .await
        .expect("Stack call failed!");

    tracing::info!("{:?}", result);

    Stack::cast(pid, StackMessage::Push(String::from("rust")));

    let result = Stack::call(pid, StackMessage::Pop, None)
        .await
        .expect("Stack call failed!");

    tracing::info!("{:?}", result);
}
