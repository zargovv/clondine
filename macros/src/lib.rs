mod ext;
mod macros;

use proc_macro::TokenStream;

#[proc_macro_derive(Cpref, attributes(cpref))]
pub fn cpref(ts: TokenStream) -> TokenStream {
  macros::cpref(ts)
}

#[proc_macro_derive(AttributeInfo)]
pub fn attribute_info(ts: TokenStream) -> TokenStream {
  macros::attribute_info(ts)
}

#[proc_macro_derive(Deread)]
pub fn deread(ts: TokenStream) -> TokenStream {
  macros::deread(ts)
}
