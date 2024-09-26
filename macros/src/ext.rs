use std::iter::{self, Peekable};

use proc_macro::{Delimiter, TokenStream, TokenTree};

pub(crate) fn parse_attr(ts: &mut Peekable<impl Iterator<Item = TokenTree>>) -> TokenStream {
  let mut attr = TokenStream::new();
  while matches!(ts.peek(), Some(TokenTree::Punct(p)) if p.as_char() == '#') {
    let Some(token) = ts.next() else {
      unreachable!();
    };
    attr.extend(iter::once(token));

    let Some(TokenTree::Group(g)) = ts.next() else {
      unreachable!("Attr group expected");
    };
    assert_eq!(g.delimiter(), Delimiter::Bracket);
    attr.extend(iter::once(TokenTree::Group(g)));
  }
  attr
}

pub(crate) fn parse_vis(ts: &mut Peekable<impl Iterator<Item = TokenTree>>) -> TokenStream {
  let mut vis = TokenStream::new();
  if matches!(ts.peek(), Some(TokenTree::Ident(id)) if id.to_string() == "pub") {
    let Some(token) = ts.next() else {
      unreachable!();
    };
    vis.extend(iter::once(token));
    if let Some(TokenTree::Group(g)) = ts.peek() {
      assert_eq!(g.delimiter(), Delimiter::Parenthesis);
      let Some(t) = ts.next() else {
        unreachable!();
      };
      vis.extend(iter::once(t));
    }
  }
  vis
}
