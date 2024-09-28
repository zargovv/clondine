use std::iter::Peekable;

use proc_macro::{Delimiter, Ident, TokenStream, TokenTree};
use quote::quote;

use crate::{ext, field::Field};

struct Variant {
  ident: TokenTree,
  fields: Vec<Field>,
  tag: TokenTree,
}

impl Variant {
  fn from_iter(mut iter: impl Iterator<Item = TokenTree>) -> Vec<Self> {
    let mut v = Vec::new();

    while let Some(ident) = iter.next() {
      let TokenTree::Ident(ident) = ident else {
        unreachable!("Variant ident expected");
      };
      let Some(TokenTree::Group(group)) = iter.next() else {
        unreachable!("Group expected");
      };
      assert_eq!(group.delimiter(), Delimiter::Brace);

      let fields = Field::from_iter(&mut group.stream().into_iter().peekable());

      let Some(TokenTree::Punct(punct)) = iter.next() else {
        unreachable!("Punct expected");
      };
      assert_eq!(punct.as_char(), '=');

      let Some(TokenTree::Literal(tag)) = iter.next() else {
        unreachable!("Literal expected");
      };

      let Some(TokenTree::Punct(punct)) = iter.next() else {
        unreachable!("Punct expected");
      };
      assert_eq!(punct.as_char(), ',');

      v.push(Self {
        ident: TokenTree::Ident(ident),
        fields,
        tag: TokenTree::Literal(tag),
      });
    }

    v
  }
}

fn cpref_enum(
  attr: TokenStream,
  vis: TokenStream,
  ts: &mut Peekable<impl Iterator<Item = TokenTree>>,
) -> TokenStream {
  let Some(TokenTree::Ident(ident)) = ts.next() else {
    unreachable!("Enum ident expected");
  };

  let debug_ident = TokenTree::Ident(Ident::new(&format!("{ident}Debug"), ident.span()));
  let ident = TokenTree::Ident(ident);

  let Some(TokenTree::Group(group)) = ts.next() else {
    unreachable!("Enum body expected");
  };
  assert_eq!(group.delimiter(), Delimiter::Brace);

  let variants = Variant::from_iter(&mut group.stream().into_iter());

  let debug_enum_variants = variants
    .iter()
    .map(|v| {
      let fields = v
        .fields
        .iter()
        .map(|f| {
          let ident = &f.ident;
          let ty = if let Some(with_ty) = &f.with_ty {
            with_ty
          } else {
            &f.ty
          };
          quote! {
            #ident: #ty,
          }
        })
        .collect::<TokenStream>();

      let ident = &v.ident;
      quote! {
        #ident {
          #fields
        },
      }
    })
    .collect::<TokenStream>();

  let debug_arms = variants
    .iter()
    .map(|v| {
      let fields = v
        .fields
        .iter()
        .map(|f| {
          let ident = &f.index_ident;
          quote! {
            #ident,
          }
        })
        .collect::<TokenStream>();
      let debug_fields = v
        .fields
        .iter()
        .map(|f| {
          let index_ident = &f.index_ident;
          let ident = &f.ident;
          if f.cpref {
            if f.with_ty.is_some() {
              quote! {
                #ident: crate::classfile::CpDebug::debug(#index_ident, pool),
              }
            } else {
              quote! {
                #ident: {
                  let Some(cp) = pool.get(usize::from(*#index_ident)) else {
                    unreachable!();
                  };
                  Box::new(crate::classfile::CpDebug::debug(&**cp, pool))
                },
              }
            }
          } else {
            quote! {
              #ident,
            }
          }
        })
        .collect::<TokenStream>();

      let ident = &v.ident;
      quote! {
        Self::#ident {
          #fields
        } => #debug_ident::#ident {
          #debug_fields
        },
      }
    })
    .collect::<TokenStream>();

  let deread_arms = variants
    .iter()
    .map(|v| {
      let fields = v
        .fields
        .iter()
        .map(|f| {
          let ident = &f.index_ident;
          quote! {
            #ident: r.deread()?,
          }
        })
        .collect::<TokenStream>();

      let ident = &v.ident;
      let tag = &v.tag;
      quote! {
        #tag => Self::#ident {
          #fields
        },
      }
    })
    .collect::<TokenStream>();

  quote! {
    #attr
    #[derive(Debug)]
    #vis enum #debug_ident<'a> {
      #debug_enum_variants
    }

    impl<'a> crate::classfile::CpDebug<'a> for #ident {
      type Output = #debug_ident<'a>;

      fn debug(&'a self, pool: &'a crate::classfile::ConstantPool) -> Self::Output {
        match self {
          #debug_arms
        }
      }
    }

    impl crate::deread::Deread for #ident {
      fn deread(mut r: impl crate::deread::Dereader) -> ::std::io::Result<Self> {
        Ok(match r.deread::<u8>()? {
          #deread_arms
          tag => return Err(::std::io::Error::new(
            ::std::io::ErrorKind::InvalidData,
            format!("invalid tag: {}", tag),
          )),
        })
      }
    }
  }
}

fn cpref_struct(
  attr: TokenStream,
  vis: TokenStream,
  ts: &mut Peekable<impl Iterator<Item = TokenTree>>,
) -> TokenStream {
  let Some(TokenTree::Ident(ident)) = ts.next() else {
    unreachable!("Struct ident expected");
  };

  let debug_ident = TokenTree::Ident(Ident::new(&format!("{ident}Debug"), ident.span()));
  let ident = TokenTree::Ident(ident);

  let Some(TokenTree::Group(group)) = ts.next() else {
    unreachable!("Struct body expected");
  };
  assert_eq!(group.delimiter(), Delimiter::Brace);

  let fields = Field::from_iter(&mut group.stream().into_iter().peekable());

  let debug_struct_fields = fields
    .iter()
    .map(|f| {
      let ident = &f.ident;
      let ty = if let Some(with_ty) = &f.with_ty {
        with_ty
      } else {
        &f.ty
      };
      quote! {
        #ident: #ty,
      }
    })
    .collect::<TokenStream>();

  let debug_fields = fields
    .iter()
    .map(|f| {
      let index_ident = &f.index_ident;
      let ident = &f.ident;
      if f.cpref {
        if f.with_ty.is_some() {
          quote! {
            #ident: crate::classfile::CpDebug::debug(&self.#index_ident, pool),
          }
        } else {
          quote! {
            #ident: {
              let Some(cp) = pool.get(usize::from(self.#index_ident)) else {
                unreachable!();
              };
              Box::new(crate::classfile::CpDebug::debug(&**cp, pool))
            },
          }
        }
      } else {
        quote! {
          #ident: &self.#index_ident,
        }
      }
    })
    .collect::<TokenStream>();

  let deread_fields = fields
    .iter()
    .map(|f| {
      let ident = &f.index_ident;
      quote! {
        #ident: r.deread()?,
      }
    })
    .collect::<TokenStream>();

  quote! {
    #attr
    #[derive(Debug)]
    #vis struct #debug_ident<'a> {
      #debug_struct_fields
    }

    impl<'a> crate::classfile::CpDebug<'a> for #ident {
      type Output = #debug_ident<'a>;

      #vis fn debug(&'a self, pool: &'a crate::classfile::ConstantPool) -> Self::Output {
        #debug_ident { #debug_fields }
      }
    }

    impl crate::deread::Deread for #ident {
      fn deread(mut r: impl crate::deread::Dereader) -> ::std::io::Result<Self> {
        Ok(Self { #deread_fields })
      }
    }
  }
}

pub(crate) fn cpref(ts: TokenStream) -> TokenStream {
  let mut stream_iter = ts.into_iter().peekable();

  let attr = ext::parse_attr(&mut stream_iter);
  let vis = ext::parse_vis(&mut stream_iter);

  let Some(TokenTree::Ident(t)) = stream_iter.next() else {
    unreachable!("Keyword expected");
  };
  match &*t.to_string() {
    "enum" => cpref_enum(attr, vis, &mut stream_iter),
    "struct" => cpref_struct(attr, vis, &mut stream_iter),
    token => unreachable!("Unexpected ident {token:?}"),
  }
}
