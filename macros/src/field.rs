use std::iter::{self, Peekable};

use proc_macro::{Delimiter, Ident, TokenStream, TokenTree};
use quote::quote;

use crate::ext;

pub(crate) struct Field {
  pub(crate) ident: TokenTree,
  pub(crate) index_ident: TokenTree,
  pub(crate) ty: TokenStream,
  pub(crate) cpref: bool,
  pub(crate) with_ty: Option<TokenStream>,
}

impl Field {
  pub(crate) fn from_iter(iter: &mut Peekable<impl Iterator<Item = TokenTree>>) -> Vec<Self> {
    let mut f = Vec::new();
    while iter.peek().is_some() {
      let mut with_ty = None;

      let attr = ext::parse_attr(iter).into_iter().nth(1);
      let cpref = matches!(
        &attr,
        Some(TokenTree::Group(g)) if matches!(
          g.stream().into_iter().next(),
          Some(TokenTree::Ident(id)) if id.to_string() == "cpref",
        ),
      );
      if cpref {
        let Some(TokenTree::Group(g)) = &attr else {
          unreachable!();
        };
        if let Some(TokenTree::Group(g)) = g.stream().into_iter().nth(1) {
          assert_eq!(g.delimiter(), Delimiter::Parenthesis);
          let mut gstream_iter = g.stream().into_iter();
          while let Some(token) = gstream_iter.next() {
            let TokenTree::Ident(ident) = token else {
              unreachable!("Ident expected in attribute group");
            };
            match &*ident.to_string() {
              "with" => {
                let mut ts = TokenStream::new();

                let Some(TokenTree::Punct(p)) = gstream_iter.next() else {
                  unreachable!("Punct expected in attribute {ident:?}");
                };
                assert_eq!(p.as_char(), '=');

                loop {
                  let Some(token) = gstream_iter.next() else {
                    break;
                  };
                  match token {
                    TokenTree::Punct(punct) if punct.as_char() == ',' => break,
                    token => ts.extend(iter::once(token)),
                  }
                }

                with_ty = Some(ts);
              }
              v => unreachable!("Unexpected attribute {v}"),
            }
          }
        }
      }

      let Some(TokenTree::Ident(ident)) = iter.next() else {
        unreachable!("Field ident expected");
      };
      let (ident, index_ident) = match (cpref, ident.to_string().strip_suffix("_index")) {
        (true, Some(s)) => (
          TokenTree::Ident(Ident::new(s, ident.span())),
          TokenTree::Ident(ident),
        ),
        _ => (TokenTree::Ident(ident.clone()), TokenTree::Ident(ident)),
      };

      let Some(TokenTree::Punct(punct)) = iter.next() else {
        unreachable!("Punct expected");
      };
      assert_eq!(punct.as_char(), ':');

      let mut ty = TokenStream::new();
      loop {
        let Some(token) = iter.next() else {
          unreachable!("Unexpected EOF");
        };
        match token {
          TokenTree::Punct(punct) if punct.as_char() == ',' => break,
          token => ty.extend(iter::once(token)),
        }
      }

      f.push(Self {
        ident,
        index_ident,
        ty: if cpref {
          quote! {
            Box<crate::classfile::cpinfo::CpInfoDebug<'a>>
          }
        } else {
          let mut ts = quote! {
            &'a
          };
          ts.extend(ty.clone());
          ts
        },
        cpref,
        with_ty,
      })
    }
    f
  }
}
