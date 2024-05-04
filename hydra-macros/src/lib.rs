use proc_macro::TokenStream;

mod entry;

/// Marks an async function to be executed as a hydra application.
///
/// Note: This macro is designed to be simplistic, if you wish to customize your application, you should
/// implement the trait yourself and call `run` instead.
///
/// If the feature `tracing` is enabled, this will automatically setup a subscriber and panic hook.
#[proc_macro_attribute]
pub fn main(arg: TokenStream, item: TokenStream) -> TokenStream {
    entry::main(arg, item)
}

/// Marks an async function to be executed as a hydra application suitable for the test environment.
#[proc_macro_attribute]
pub fn test(arg: TokenStream, item: TokenStream) -> TokenStream {
    entry::test(arg, item)
}
