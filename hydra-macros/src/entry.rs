use proc_macro::TokenStream;

use quote::format_ident;
use quote::quote;

use syn::parse_macro_input;
use syn::ItemFn;

pub(crate) fn main(_: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let input_name = &input_fn.sig.ident;
    let input_block = &input_fn.block;
    let input_struct = format_ident!("{}_MainStruct", input_name);

    let tracing = cfg!(feature = "tracing");
    let tracing = tracing.then(|| {
        quote! {
            const TRACING_SUBSCRIBE: bool = true;
            const TRACING_PANICS: bool = true;
        }
    });

    let output = quote! {
        #[allow(non_camel_case_types)]
        struct #input_struct;

        impl ::hydra::Application for #input_struct {
            #tracing

            async fn start(&self) -> Result<::hydra::Pid, ::hydra::ExitReason> {
                Ok(Process::spawn_link(async move {
                    #input_block
                }))
            }
        }

        fn #input_name() {
            use ::hydra::Application;

            let main = #input_struct;
            main.run();
        }
    };

    output.into()
}

pub(crate) fn test(_: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let input_name = &input_fn.sig.ident;
    let input_block = &input_fn.block;
    let input_struct = format_ident!("{}_TestStruct", input_name);

    let output = quote! {
        #[allow(non_camel_case_types)]
        struct #input_struct;

        impl ::hydra::Application for #input_struct {
            async fn start(&self) -> Result<::hydra::Pid, ::hydra::ExitReason> {
                Ok(Process::spawn_link(async move {
                    #input_block
                }))
            }
        }

        #[test]
        fn #input_name() {
            use ::hydra::Application;

            let test = #input_struct;
            test.test();
        }
    };

    output.into()
}
