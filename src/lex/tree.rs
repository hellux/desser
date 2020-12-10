use crate::lex::cook::{Delim, Token, TokenCooker, TokenKind};
use crate::lex::{LResult, LexError, LexErrorKind, Spacing, Span};
use crate::sym;

#[derive(Debug)]
pub enum TokenTree {
    Token(Token),
    Delimited(Delim, TokenStream),
}

pub type TreeAndSpace = (TokenTree, Spacing);
pub type TokenStream = Vec<TreeAndSpace>;

struct TokenTreesReader<'a> {
    cooker: TokenCooker<'a>,
    token: Token,
}

pub fn parse_token_trees<'a>(
    symtab: sym::SymbolTable,
    src: &'a str,
) -> (LResult<TokenStream>, sym::SymbolTable, Vec<LexError>) {
    let mut ttr = TokenTreesReader {
        cooker: TokenCooker::new(symtab, src),
        token: Token::dummy(),
    };

    let stream = ttr.parse_all();

    let cooker = ttr.consume();
    let (symtab, errors) = cooker.consume();

    (stream, symtab, errors)
}

impl<'a> TokenTreesReader<'a> {
    fn consume(self) -> TokenCooker<'a> {
        self.cooker
    }

    fn parse_all(&mut self) -> LResult<TokenStream> {
        let mut stream = Vec::new();

        self.eat();
        while self.token.kind != TokenKind::Eof {
            stream.push(self.parse_token_tree()?);
        }

        Ok(stream)
    }

    fn parse_token_tree(&mut self) -> LResult<TreeAndSpace> {
        let span_start = self.token.span;
        match self.token.kind {
            TokenKind::OpenDelim(delim) => {
                self.eat();

                let stream = self.parse_until_close_delim()?;
                let span = Span(span_start.0, self.token.span.1);

                match self.token.kind {
                    TokenKind::CloseDelim(d) if d == delim => {
                        self.eat();
                    }
                    TokenKind::CloseDelim(_) => {
                        return Err(LexError {
                            kind: LexErrorKind::UnmatchedDelim,
                            span,
                        });
                    }
                    TokenKind::Eof => {
                        return Err(LexError {
                            kind: LexErrorKind::UnclosedDelim,
                            span,
                        })
                    }
                    _ => {}
                };

                Ok((TokenTree::Delimited(delim, stream), Spacing::Alone))
            }
            TokenKind::CloseDelim(_) => Err(LexError {
                kind: LexErrorKind::UnexpectedCloseDelim,
                span: span_start,
            }),
            _ => {
                let tt = TokenTree::Token(self.token.take());
                let spacing = self.eat();
                Ok((tt, spacing))
            }
        }
    }

    fn parse_until_close_delim(&mut self) -> LResult<TokenStream> {
        let mut stream = Vec::new();

        loop {
            if let TokenKind::CloseDelim(_) = self.token.kind {
                return Ok(stream);
            } else {
                stream.push(self.parse_token_tree()?);
            }
        }
    }

    fn eat(&mut self) -> Spacing {
        let (spacing, token) = self.cooker.next_token();
        self.token = token;
        spacing
    }
}
