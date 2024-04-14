use proc_macro::TokenStream;

mod entry;

/// A macro.
#[proc_macro_attribute]
#[cfg(not(test))]
pub fn main(arg: TokenStream, item: TokenStream) -> TokenStream {
    entry::main(arg, item)
}

/// A test macro.
#[proc_macro_attribute]
pub fn test(arg: TokenStream, item: TokenStream) -> TokenStream {
    entry::test(arg, item)
}
