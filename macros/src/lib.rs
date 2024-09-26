mod ext;
mod field;
mod macros;

use proc_macro::TokenStream;

#[proc_macro_derive(Cpref, attributes(cpref))]
pub fn cpref(ts: TokenStream) -> TokenStream {
  macros::cpref(ts)
}
