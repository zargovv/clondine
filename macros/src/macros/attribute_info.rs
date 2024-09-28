use proc_macro::{Literal, TokenStream, TokenTree};
use quote::quote;

use crate::ext::Enum;

pub(crate) fn attribute_info(ts: TokenStream) -> TokenStream {
  let enum_ = Enum::from(ts);

  let vis = enum_.vis;
  let ident = enum_.ident;
  let debug_arms = enum_
    .variants
    .iter()
    .take(enum_.variants.len() - 1)
    .map(|v| {
      let variant_ident = &v.ident;
      let variant_ident_str = TokenTree::Literal(Literal::string(&variant_ident.to_string()));
      let variant_fields = v
        .fields
        .iter()
        .map(|f| {
          let ident = &f.accessor;
          let ty = &f.ty;
          quote! {
            #ident: {
              let Ok(vdbg) = <#ty as crate::deread::Deread>::deread(&mut r) else {
                unreachable!("Invalid attribute info");
              };
              vdbg
            },
          }
        })
        .collect::<TokenStream>();
      quote! {
        #variant_ident_str => #ident::#variant_ident { #variant_fields },
      }
    })
    .collect::<TokenStream>();
  quote! {
    #vis struct AttributeInfo {
      attribute_name_index: u16,
      info: crate::deread::ByteSeq,
    }

    impl crate::deread::Deread for AttributeInfo {
      fn deread(mut r: impl crate::deread::Dereader) -> ::std::io::Result<Self> {
        Ok(Self {
          attribute_name_index: r.deread()?,
          info: r.deread()?,
        })
      }
    }

    impl crate::classfile::CpDebug<'_> for AttributeInfo {
      type Output = #ident;

      fn debug(&self, pool: &crate::classfile::ConstantPool) -> Self::Output {
        let Some(cp) = pool.get(self.attribute_name_index) else {
          unreachable!("Attribute name index must be in constant pool");
        };
        let crate::classfile::CpInfo::Utf8 { bytes: name } = &**cp else {
          unreachable!("Attribute name must be a valid utf8");
        };

        let mut r = &*self.info;
        match &**name {
          #debug_arms
          // name => unreachable!("Invalid attribute name {name:?}"),
          name => #ident::Unknown {
            name: name.to_owned(),
            info: self.info.to_owned(),
          },
        }
      }
    }
  }
}
