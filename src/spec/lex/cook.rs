use crate::{Sym, SymbolTable};

use super::raw;
use super::Span;
use super::{LError, LErrorKind};

use self::TokKind::*;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokKind, span: Span) -> Self {
        Token { kind, span }
    }

    pub fn dummy() -> Self {
        Token {
            kind: TokKind::Unknown,
            span: Span(0, 0),
        }
    }

    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Token::dummy())
    }
}

#[derive(Clone, Debug)]
pub enum TokKind {
    OpenDelim(Delim),
    CloseDelim(Delim),

    Literal(LitKind),
    Keyword(Keyword),
    Ident(Sym),
    Symbol(raw::Symbol),

    Unknown,
    Eof,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Keyword {
    Def,
    Let,
    Const,
    If,
    Else,
    For,
    In,
    Constrain,
    Debug,
}

#[derive(Clone, Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Sym,
}

#[derive(Clone, Debug)]
pub enum LitKind {
    Int(i64),
    Char(u8),
    Str(Vec<u8>),
}

#[derive(Clone, Debug)]
pub(super) struct TokenCooker<'a> {
    symtab: SymbolTable,
    pos: usize,
    src: &'a str,
    errors: Vec<LError>,
}

impl<'a> TokenCooker<'a> {
    pub fn new(symtab: SymbolTable, src: &'a str) -> Self {
        TokenCooker {
            symtab,
            pos: 0,
            src,
            errors: Vec::new(),
        }
    }

    pub fn consume(self) -> (SymbolTable, Vec<LError>) {
        (self.symtab, self.errors)
    }

    fn err(&mut self, kind: LErrorKind, start: usize) {
        self.errors.push(LError {
            kind,
            span: Span::new(start, self.pos),
        });
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            let remaining_src = &self.src[self.pos..];
            if remaining_src.is_empty() {
                return Token::new(
                    TokKind::Eof,
                    Span::new(self.pos, self.pos + 1),
                );
            }

            let start = self.pos;
            let token = raw::first_token(remaining_src);
            self.pos += token.len;

            if let Some(kind) = self.cook_token(token.kind, start) {
                return Token::new(kind, Span::new(start, self.pos));
            }
        }
    }

    fn cook_token(
        &mut self,
        raw: raw::TokenKind,
        start: usize,
    ) -> Option<TokKind> {
        match raw {
            raw::TokenKind::LineComment | raw::TokenKind::Whitespace => None,
            raw::TokenKind::BlockComment { closed } => {
                if !closed {
                    self.err(LErrorKind::UnclosedBlockComment, start);
                }
                None
            }
            raw::TokenKind::OpenParen => Some(OpenDelim(Delim::Paren)),
            raw::TokenKind::CloseParen => Some(CloseDelim(Delim::Paren)),
            raw::TokenKind::OpenBrace => Some(OpenDelim(Delim::Brace)),
            raw::TokenKind::CloseBrace => Some(CloseDelim(Delim::Brace)),
            raw::TokenKind::OpenBracket => Some(OpenDelim(Delim::Bracket)),
            raw::TokenKind::CloseBracket => Some(CloseDelim(Delim::Bracket)),
            raw::TokenKind::Literal(kind) => {
                let litkind = match kind {
                    raw::LiteralKind::Int(base) => self.cook_int(start, base),
                    raw::LiteralKind::Char { closed } => {
                        self.cook_char(start, closed)
                    }
                    raw::LiteralKind::Str { closed } => {
                        self.cook_str(start, closed)
                    }
                };
                Some(Literal(litkind))
            }
            raw::TokenKind::Ident => Some(self.cook_ident(start)),
            raw::TokenKind::Sym(s) => Some(TokKind::Symbol(s)),
            raw::TokenKind::Unknown => {
                self.err(LErrorKind::UnknownToken, start);
                Some(Unknown)
            }
        }
    }

    fn cook_int(&mut self, start: usize, base: raw::Base) -> LitKind {
        let skip = match base {
            raw::Base::Decimal => 0,
            _ => 2,
        };
        let content = &self.src[start + skip..self.pos];
        let int = i64::from_str_radix(content, base.radix());
        if let Ok(val) = int {
            LitKind::Int(val)
        } else {
            self.err(LErrorKind::InvalidIntLiteral, start);
            LitKind::Int(0)
        }
    }

    fn cook_char(&mut self, start: usize, closed: bool) -> LitKind {
        if !closed {
            self.err(LErrorKind::UnclosedCharLiteral, start);
        }

        let bytes = self.get_str_bytes(start);

        if let [b] = bytes.as_slice() {
            LitKind::Char(*b)
        } else {
            self.err(LErrorKind::InvalidCharLiteral, start);
            LitKind::Char(0)
        }
    }

    fn cook_str(&mut self, start: usize, closed: bool) -> LitKind {
        if !closed {
            self.err(LErrorKind::UnclosedStrLiteral, start);
        }

        let bytes = self.get_str_bytes(start);

        LitKind::Str(bytes)
    }

    fn get_str_bytes(&mut self, start: usize) -> Vec<u8> {
        let content = &mut self.src[start + 1..self.pos - 1].chars();
        let mut bytes = Vec::new();
        loop {
            match &content.next() {
                Some('\\') => match content.next() {
                    Some('n') => bytes.push(b'\n'),
                    Some('t') => bytes.push(b'\t'),
                    Some('r') => bytes.push(b'\r'),
                    Some('0') => bytes.push(b'\0'),
                    _ => self.err(LErrorKind::InvalidCharLiteral, start),
                },
                Some(c) => bytes.push(*c as u8),
                _ => break,
            }
        }
        bytes
    }

    fn cook_ident(&mut self, start: usize) -> TokKind {
        let id = &self.src[start..self.pos];

        match id {
            "def" => Keyword(Keyword::Def),
            "const" => Keyword(Keyword::Const),
            "let" => Keyword(Keyword::Let),
            "if" => Keyword(Keyword::If),
            "else" => Keyword(Keyword::Else),
            "for" => Keyword(Keyword::For),
            "in" => Keyword(Keyword::In),
            "debug" => Keyword(Keyword::Debug),
            "constrain" => Keyword(Keyword::Constrain),

            id => Ident(self.symtab.insert(id)),
        }
    }
}

#[cfg(test)]
pub fn tokenize(src: &str) -> impl Iterator<Item = Token> + '_ {
    let symtab = SymbolTable::new();
    let mut cooker = TokenCooker::new(symtab, src);
    let mut empty = false;
    std::iter::from_fn(move || {
        if empty {
            None
        } else {
            let token = cooker.next_token();
            empty = matches!(token.kind, Eof);
            Some(token)
        }
    })
}
