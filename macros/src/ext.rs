use std::{
  iter::{self, Peekable},
  slice,
};

use proc_macro::{Delimiter, Literal, TokenStream, TokenTree};

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

#[allow(dead_code)]
pub(crate) struct Field {
  pub(crate) attr: TokenStream,
  pub(crate) vis: TokenStream,
  pub(crate) accessor: TokenTree,
  pub(crate) ty: TokenStream,
}

impl Field {
  fn from_iter(
    stream_iter: &mut Peekable<impl Iterator<Item = TokenTree>>,
    tuple: bool,
  ) -> Vec<Self> {
    let mut fields = Vec::new();
    let mut i = 0;
    while stream_iter.peek().is_some() {
      let attr = parse_attr(stream_iter);
      let vis = parse_vis(stream_iter);

      let accessor = if tuple {
        let lit = TokenTree::Literal(Literal::i32_unsuffixed(i));
        i += 1;
        lit
      } else {
        let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
          unreachable!("Struct field ident expected");
        };

        let Some(TokenTree::Punct(token)) = stream_iter.next() else {
          unreachable!("`:` expected: {ident}");
        };
        assert_eq!(token.as_char(), ':');

        TokenTree::Ident(ident)
      };

      let mut ty = TokenStream::new();
      while let Some(token) = stream_iter.next() {
        match token {
          TokenTree::Punct(p) if p.as_char() == ',' => break,
          token => ty.extend(iter::once(token)),
        }
      }

      fields.push(Self {
        attr,
        vis,
        accessor,
        ty,
      })
    }
    fields
  }
}

pub(crate) enum FieldIterKind<'a> {
  None,
  Tuple(slice::Iter<'a, Field>),
  Block(slice::Iter<'a, Field>),
}

impl<'a> Iterator for FieldIterKind<'a> {
  type Item = &'a Field;

  fn next(&mut self) -> Option<Self::Item> {
    match self {
      Self::None => None,
      Self::Tuple(iter) => iter.next(),
      Self::Block(iter) => iter.next(),
    }
  }
}

pub(crate) enum FieldDescriptor {
  None,
  Tuple(Vec<Field>),
  Block(Vec<Field>),
}

impl FieldDescriptor {
  pub(crate) fn iter(&self) -> FieldIterKind {
    match self {
      Self::None => FieldIterKind::None,
      Self::Tuple(fields) => FieldIterKind::Tuple(fields.iter()),
      Self::Block(fields) => FieldIterKind::Block(fields.iter()),
    }
  }
}

impl From<Option<TokenTree>> for FieldDescriptor {
  fn from(value: Option<TokenTree>) -> Self {
    let Some(token) = value else {
      return Self::None;
    };

    let TokenTree::Group(group) = token else {
      unreachable!("Definition body expected");
    };

    let tuple = match group.delimiter() {
      Delimiter::Brace => false,
      Delimiter::Parenthesis => true,
      _ => unreachable!("Definition body expected"),
    };

    let fields = Field::from_iter(&mut group.stream().into_iter().peekable(), tuple);
    if tuple {
      Self::Tuple(fields)
    } else {
      Self::Block(fields)
    }
  }
}

#[allow(dead_code)]
pub(crate) struct Struct {
  pub(crate) attr: TokenStream,
  pub(crate) vis: TokenStream,
  pub(crate) ident: TokenTree,
  pub(crate) fields: FieldDescriptor,
}

impl From<TokenStream> for Struct {
  fn from(value: TokenStream) -> Self {
    let mut stream_iter = value.into_iter().peekable();

    let attr = parse_attr(&mut stream_iter);
    let vis = parse_vis(&mut stream_iter);

    let Some(TokenTree::Ident(token)) = stream_iter.next() else {
      unreachable!("Keyword `struct` expected");
    };
    assert_eq!(token.to_string(), "struct");

    let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
      unreachable!("Struct ident expected");
    };

    let fields = FieldDescriptor::from(stream_iter.next());

    Self {
      attr,
      vis,
      ident: TokenTree::Ident(ident),
      fields,
    }
  }
}

#[allow(dead_code)]
pub(crate) struct Variant {
  pub(crate) attr: TokenStream,
  pub(crate) ident: TokenTree,
  pub(crate) fields: FieldDescriptor,
}

#[allow(dead_code)]
pub(crate) struct Enum {
  pub(crate) attr: TokenStream,
  pub(crate) vis: TokenStream,
  pub(crate) ident: TokenTree,
  pub(crate) variants: Vec<Variant>,
}

impl From<TokenStream> for Enum {
  fn from(value: TokenStream) -> Self {
    let mut stream_iter = value.into_iter().peekable();

    let attr = parse_attr(&mut stream_iter);
    let vis = parse_vis(&mut stream_iter);

    let Some(TokenTree::Ident(token)) = stream_iter.next() else {
      unreachable!("Keyword `enum` expected");
    };
    assert_eq!(token.to_string(), "enum");

    let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
      unreachable!("Enum ident expected");
    };
    let Some(TokenTree::Group(group)) = stream_iter.next() else {
      unreachable!("Enum body expected");
    };
    assert_eq!(group.delimiter(), Delimiter::Brace);

    let mut stream_iter = group.stream().into_iter().peekable();

    let mut variants = Vec::new();
    while stream_iter.peek().is_some() {
      let attr = parse_attr(&mut stream_iter);

      let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
        unreachable!("Enum variant ident expected");
      };
      let fields = FieldDescriptor::from(stream_iter.next());

      if matches!(stream_iter.peek(), Some(TokenTree::Punct(p)) if p.as_char() == ',') {
        stream_iter.next();
      }

      variants.push(Variant {
        attr,
        ident: TokenTree::Ident(ident),
        fields,
      });
    }

    Self {
      attr,
      vis,
      ident: TokenTree::Ident(ident),
      variants,
    }
  }
}
