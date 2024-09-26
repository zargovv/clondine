use std::iter::{self, Peekable};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

#[derive(Clone, Copy)]
enum GroupTokenStreamKind {
  Empty,
  Single,
  Multi,
}

impl From<&Group> for GroupTokenStreamKind {
  fn from(value: &Group) -> Self {
    match value.stream().into_iter().take(2).count() {
      0 => Self::Empty,
      1 => Self::Single,
      _ => Self::Multi,
    }
  }
}

fn quote_tt(tt: TokenTree, ts: &mut Peekable<impl Iterator<Item = TokenTree>>) -> TokenStream {
  TokenStream::from_iter([
    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    TokenTree::Ident(Ident::new("TokenStream", Span::call_site())),
    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    TokenTree::Ident(Ident::new("from", Span::call_site())),
    TokenTree::Group(Group::new(
      Delimiter::Parenthesis,
      TokenStream::from_iter(match tt {
        TokenTree::Group(group) => {
          let gtsk = GroupTokenStreamKind::from(&group);
          [
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Punct(Punct::new(':', Spacing::Alone)),
            TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Punct(Punct::new(':', Spacing::Alone)),
            TokenTree::Ident(Ident::new("TokenTree", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Punct(Punct::new(':', Spacing::Alone)),
            TokenTree::Ident(Ident::new("Group", Span::call_site())),
            TokenTree::Group(Group::new(
              Delimiter::Parenthesis,
              TokenStream::from_iter([
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Ident(Ident::new("Group", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Ident(Ident::new("new", Span::call_site())),
                TokenTree::Group(Group::new(
                  Delimiter::Parenthesis,
                  TokenStream::from_iter([
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("Delimiter", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                    TokenTree::Ident(Ident::new(
                      match group.delimiter() {
                        Delimiter::Parenthesis => "Parenthesis",
                        Delimiter::Brace => "Brace",
                        Delimiter::Bracket => "Bracket",
                        Delimiter::None => "None",
                      },
                      Span::call_site(),
                    )),
                    TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("TokenStream", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                    TokenTree::Ident(Ident::new(
                      match gtsk {
                        GroupTokenStreamKind::Empty => "new",
                        GroupTokenStreamKind::Single => "from",
                        GroupTokenStreamKind::Multi => "from_iter",
                      },
                      Span::call_site(),
                    )),
                    TokenTree::Group(Group::new(Delimiter::Parenthesis, {
                      let mut iter = group.stream().into_iter().peekable();
                      let mut ts = TokenStream::new();
                      while let Some(tt) = iter.next() {
                        ts.extend(quote_tt(tt, &mut iter).into_iter().chain(iter::once(
                          TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                        )));
                      }

                      if let GroupTokenStreamKind::Multi = gtsk {
                        TokenStream::from(TokenTree::Group(Group::new(Delimiter::Bracket, ts)))
                      } else {
                        ts
                      }
                    })),
                  ]),
                )),
              ]),
            )),
          ]
        }
        TokenTree::Ident(ident) => [
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("TokenTree", Span::call_site())),
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("Ident", Span::call_site())),
          TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("Ident", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("new", Span::call_site())),
              TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([
                  TokenTree::Literal(Literal::string(&ident.to_string())),
                  TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                  TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                  TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                  TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
                  TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                  TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                  TokenTree::Ident(Ident::new("Span", Span::call_site())),
                  TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                  TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                  TokenTree::Ident(Ident::new("call_site", Span::call_site())),
                  TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                ]),
              )),
            ]),
          )),
        ],
        TokenTree::Punct(punct)
          if punct.as_char() == '#'
            && (matches!(ts.peek(), Some(TokenTree::Ident(_)))
              || matches!(ts.peek(), Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis)) =>
        {
          return match ts.next() {
            Some(TokenTree::Group(_group)) => {
              unimplemented!()
            }
            Some(TokenTree::Ident(ident)) => TokenStream::from_iter([
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("TokenStream", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("from_iter", Span::call_site())),
              TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([TokenTree::Group(Group::new(
                  Delimiter::Bracket,
                  TokenStream::from_iter([
                    TokenTree::Ident(ident),
                    TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("clone", Span::call_site())),
                    TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                  ]),
                ))]),
              )),
            ]),
            _ => unreachable!(),
          }
        }
        TokenTree::Punct(punct) => [
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("TokenTree", Span::call_site())),
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("Punct", Span::call_site())),
          TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("Punct", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("new", Span::call_site())),
              TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([
                  TokenTree::Literal(Literal::character(punct.as_char())),
                  TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                  TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                  TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                  TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
                  TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                  TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                  TokenTree::Ident(Ident::new("Spacing", Span::call_site())),
                  TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                  TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                  TokenTree::Ident(Ident::new(
                    match punct.spacing() {
                      Spacing::Joint => "Joint",
                      Spacing::Alone => "Alone",
                    },
                    Span::call_site(),
                  )),
                ]),
              )),
            ]),
          )),
        ],
        TokenTree::Literal(literal) => [
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("TokenTree", Span::call_site())),
          TokenTree::Punct(Punct::new(':', Spacing::Joint)),
          TokenTree::Punct(Punct::new(':', Spacing::Alone)),
          TokenTree::Ident(Ident::new("Literal", Span::call_site())),
          TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            [
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
              TokenTree::Ident(Ident::new("Literal", Span::call_site())),
              TokenTree::Punct(Punct::new(':', Spacing::Joint)),
              TokenTree::Punct(Punct::new(':', Spacing::Alone)),
            ]
            .into_iter()
            .chain({
              let lit = literal.to_string();
              let (method, lit) = if lit
                .chars()
                .next()
                .map_or(false, |c| c == '-' || c.is_numeric())
              {
                (
                  "i32_unsuffixed",
                  Literal::i32_unsuffixed(match lit.parse::<i32>() {
                    Ok(v) => v,
                    Err(error) => unreachable!("{error}"),
                  }),
                )
              } else if let Some(s) = lit.strip_prefix('"') {
                let Some(s) = s.strip_suffix('"') else {
                  unreachable!();
                };
                ("string", Literal::string(&s.replace("\\\"", "\"")))
              } else {
                unimplemented!();
              };

              [
                TokenTree::Ident(Ident::new(method, Span::call_site())),
                TokenTree::Group(Group::new(
                  Delimiter::Parenthesis,
                  TokenStream::from(TokenTree::Literal(lit)),
                )),
              ]
            })
            .collect(),
          )),
        ],
      }),
    )),
  ])
}

#[proc_macro]
pub fn quote(ts: TokenStream) -> TokenStream {
  TokenStream::from_iter([
    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    TokenTree::Ident(Ident::new("proc_macro", Span::call_site())),
    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    TokenTree::Ident(Ident::new("TokenStream", Span::call_site())),
    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    TokenTree::Ident(Ident::new("from_iter", Span::call_site())),
    TokenTree::Group(Group::new(
      Delimiter::Parenthesis,
      TokenStream::from(TokenTree::Group(Group::new(Delimiter::Bracket, {
        let mut iter = ts.into_iter().peekable();
        let mut ts = TokenStream::new();
        while let Some(tt) = iter.next() {
          ts.extend(
            quote_tt(tt, &mut iter)
              .into_iter()
              .chain(iter::once(TokenTree::Punct(Punct::new(
                ',',
                Spacing::Alone,
              )))),
          );
        }
        ts
      }))),
    )),
  ])
}
