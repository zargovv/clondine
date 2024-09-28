use proc_macro::TokenStream;
use quote::quote;

use crate::ext::{self, Struct};

pub(crate) fn deread(ts: TokenStream) -> TokenStream {
  let struct_ = Struct::from(ts);

  let deread_fields = struct_
    .fields
    .iter()
    .map(
      |ext::Field {
         accessor: ident, ..
       }| {
        quote! {
          #ident: r.deread()?,
        }
      },
    )
    .collect::<TokenStream>();

  let ident = struct_.ident;
  quote! {
    impl crate::deread::Deread for #ident {
      fn deread(mut r: impl crate::deread::Dereader) -> ::std::io::Result<Self> {
        Ok(Self {
          #deread_fields
        })
      }
    }
  }
}
