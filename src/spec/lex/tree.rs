use std::collections::VecDeque;

use crate::SymbolTable;

use super::cook::{Delim, TokKind, Token, TokenCooker};
use super::{Error, Keyword, LError, LErrorKind, LResult, Span, Symbol};

#[derive(Debug, Clone)]
pub enum TokTree {
    Token(Token),
    Delim(DelimNode),
}

#[derive(Debug, Clone)]
pub struct DelimNode {
    pub delim: Delim,
    pub span: Span,
    pub stream: TokenStream,
}

impl TokTree {
    pub fn take(&mut self) -> Self {
        let mut dummy = Token::dummy();
        dummy.span = self.span();
        std::mem::replace(self, TokTree::Token(dummy))
    }

    pub fn span(&self) -> Span {
        match self {
            TokTree::Token(t) => t.span,
            TokTree::Delim(dn) => dn.span,
        }
    }

    pub fn pos(&self) -> u32 {
        self.span().0
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream(VecDeque<TokTree>);

impl TokenStream {
    pub fn peek(&self) -> Option<&TokTree> {
        self.0.get(0)
    }

    pub fn eat(&mut self) -> Option<TokTree> {
        self.0.pop_front()
    }

    pub fn span(&self) -> Span {
        let start = self.0.front().map_or(0, TokTree::pos);
        let end = self.0.back().map_or(0, |tr| tr.span().1);
        Span(start, end)
    }

    pub fn split_on(mut self, sep: Symbol) -> Vec<Self> {
        let mut streams = Vec::new();

        while self.not_empty() {
            let stream = self.eat_until_sym(sep);
            if stream.not_empty() {
                self.eat(); // throw away separator
            }
            streams.push(stream);
        }

        streams
    }

    pub fn eat_while<P>(&mut self, pred: &mut P) -> Self
    where
        P: FnMut(&TokKind) -> bool,
    {
        let mut i: usize = 0;

        loop {
            match self.0.get(i) {
                Some(TokTree::Token(token)) if !pred(&token.kind) => break,
                None => break,
                _ => {}
            }
            i += 1;
        }

        let remaining = self.0.split_off(i);
        let eaten = std::mem::replace(&mut self.0, remaining);

        TokenStream(eaten)
    }

    pub fn eat_until_sym(&mut self, symbol: Symbol) -> Self {
        self.eat_while(
            &mut |t| !matches!(t, TokKind::Symbol(s) if *s == symbol),
        )
    }

    pub fn eat_until_kw(&mut self, keyword: Keyword) -> Self {
        self.eat_while(
            &mut |t| !matches!(t, TokKind::Keyword(kw) if *kw == keyword),
        )
    }

    pub fn not_empty(&self) -> bool {
        !self.0.is_empty()
    }
}

struct TokTreesReader<'a> {
    cooker: TokenCooker<'a>,
    token: Token,
}

pub fn parse_token_trees(
    symtab: SymbolTable,
    src: &str,
) -> (Result<TokenStream, Error>, SymbolTable, Vec<Error>) {
    let mut ttr = TokTreesReader {
        cooker: TokenCooker::new(symtab, src),
        token: Token::dummy(),
    };

    let stream = ttr.parse_all();

    let cooker = ttr.consume();
    let (symtab, errors) = cooker.consume();

    (
        stream.map_err(|le| le.into()),
        symtab,
        errors.into_iter().map(|le| le.into()).collect(),
    )
}

impl<'a> TokTreesReader<'a> {
    fn consume(self) -> TokenCooker<'a> {
        self.cooker
    }

    fn parse_all(&mut self) -> LResult<TokenStream> {
        let mut trees = VecDeque::new();

        self.eat();
        while !matches!(self.token.kind, TokKind::Eof) {
            trees.push_back(self.parse_token_tree()?);
        }

        Ok(TokenStream(trees))
    }

    fn parse_token_tree(&mut self) -> LResult<TokTree> {
        let span_start = self.token.span;
        match self.token.kind {
            TokKind::OpenDelim(delim) => {
                self.eat();

                let stream = self.parse_until_close_delim()?;
                let span = Span(span_start.0, self.token.span.1);

                match self.token.kind {
                    TokKind::CloseDelim(d) if d == delim => {
                        self.eat();
                    }
                    TokKind::CloseDelim(_) => {
                        return Err(LError {
                            kind: LErrorKind::UnmatchedDelim,
                            span,
                        });
                    }
                    TokKind::Eof => {
                        return Err(LError {
                            kind: LErrorKind::UnclosedDelim,
                            span,
                        })
                    }
                    _ => {}
                };

                let dn = DelimNode {
                    delim,
                    span,
                    stream,
                };
                Ok(TokTree::Delim(dn))
            }
            TokKind::CloseDelim(_) => Err(LError {
                kind: LErrorKind::UnexpectedCloseDelim,
                span: span_start,
            }),
            _ => {
                let tt = TokTree::Token(self.token.take());
                self.eat();
                Ok(tt)
            }
        }
    }

    fn parse_until_close_delim(&mut self) -> LResult<TokenStream> {
        let mut trees = VecDeque::new();

        loop {
            match self.token.kind {
                TokKind::CloseDelim(_) => {
                    return Ok(TokenStream(trees));
                }
                TokKind::Eof => {
                    return Err(LError {
                        kind: LErrorKind::UnclosedDelim,
                        span: self.token.span,
                    });
                }
                _ => {
                    trees.push_back(self.parse_token_tree()?);
                }
            }
        }
    }

    fn eat(&mut self) {
        let token = self.cooker.next_token();
        self.token = token;
    }
}
