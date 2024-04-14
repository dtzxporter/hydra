use proc_macro::TokenStream;

use quote::quote;

use syn::parse_macro_input;
use syn::ItemFn;

#[cfg(not(test))]
pub(crate) fn main(_: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let input_name = &input_fn.sig.ident;
    let input_block = &input_fn.block;

    let output = quote! {
        #[allow(unreachable_code)]
        fn #input_name() {
            let rt = ::tokio::runtime::Runtime::new().unwrap();

            rt.block_on(async {
                let (tx, rx) = ::tokio::sync::oneshot::channel();

                ::hydra::Process::spawn(async {
                    #input_block

                    tx.send(()).unwrap();
                });

                rx.await.unwrap();
            });
        }
    };

    output.into()
}

pub(crate) fn test(_: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let input_name = &input_fn.sig.ident;
    let input_block = &input_fn.block;

    let output = quote! {
        #[allow(unreachable_code)]
        #[test]
        fn #input_name() {
            let rt = ::tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();

            rt.block_on(async {
                let (tx, rx) = ::tokio::sync::oneshot::channel();

                ::hydra::Process::spawn(async {
                    #input_block

                    tx.send(()).unwrap();
                });

                rx.await.unwrap();
            });
        }
    };

    output.into()
}
