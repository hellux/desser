use crate::lex::cook::{Delim, TokKind, Token, TokenCooker};
use crate::lex::{LError, LErrorKind, LResult, Spacing, Span};
use crate::sym;

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
    fn dummy() -> Self {
        TokTree::Token(Token::dummy())
    }

    pub fn take(&mut self) -> Self {
        std::mem::replace(self, TokTree::dummy())
    }
}

pub type TreeAndSpace = (TokTree, Spacing);

#[derive(Debug, Clone)]
pub struct TokenStream(Vec<TreeAndSpace>);

impl TokenStream {
    pub fn peek(&self) -> Option<&TokTree> {
        match self.0.get(0) {
            Some((tree, _)) => Some(&tree),
            _ => None,
        }
    }

    pub fn eat(&mut self) -> Option<TreeAndSpace> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.remove(0))
        }
    }

    pub fn eat_while_token<P>(&mut self, pred: &mut P) -> Self
    where
        P: FnMut(&TokKind) -> bool,
    {
        let mut taken = Vec::new();

        while let Some(TokTree::Token(token)) = self.peek() {
            if pred(&token.kind) {
                taken.push(self.eat().unwrap());
            } else {
                break;
            }
        }

        TokenStream(taken)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

struct TokTreesReader<'a> {
    cooker: TokenCooker<'a>,
    token: Token,
}

pub fn parse_token_trees<'a>(
    symtab: sym::SymbolTable,
    src: &'a str,
) -> (LResult<TokenStream>, sym::SymbolTable, Vec<LError>) {
    let mut ttr = TokTreesReader {
        cooker: TokenCooker::new(symtab, src),
        token: Token::dummy(),
    };

    let stream = ttr.parse_all();

    let cooker = ttr.consume();
    let (symtab, errors) = cooker.consume();

    (stream, symtab, errors)
}

impl<'a> TokTreesReader<'a> {
    fn consume(self) -> TokenCooker<'a> {
        self.cooker
    }

    fn parse_all(&mut self) -> LResult<TokenStream> {
        let mut trees = Vec::new();

        self.eat();
        while self.token.kind != TokKind::Eof {
            trees.push(self.parse_token_tree()?);
        }

        Ok(TokenStream(trees))
    }

    fn parse_token_tree(&mut self) -> LResult<TreeAndSpace> {
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
                Ok((TokTree::Delim(dn), Spacing::Alone))
            }
            TokKind::CloseDelim(_) => Err(LError {
                kind: LErrorKind::UnexpectedCloseDelim,
                span: span_start,
            }),
            _ => {
                let tt = TokTree::Token(self.token.take());
                let spacing = self.eat();
                Ok((tt, spacing))
            }
        }
    }

    fn parse_until_close_delim(&mut self) -> LResult<TokenStream> {
        let mut trees = Vec::new();

        loop {
            if let TokKind::CloseDelim(_) = self.token.kind {
                return Ok(TokenStream(trees));
            } else {
                trees.push(self.parse_token_tree()?);
            }
        }
    }

    fn eat(&mut self) -> Spacing {
        let (spacing, token) = self.cooker.next_token();
        self.token = token;
        spacing
    }
}
