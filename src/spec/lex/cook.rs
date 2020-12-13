use crate::spec::error::Span;
use crate::spec::lex::raw;
use crate::spec::lex::{LError, LErrorKind, Spacing};
use crate::sym;

use self::TokKind::*;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum TokKind {
    OpenDelim(Delim),
    CloseDelim(Delim),
    SemiColon,
    Comma,

    Literal(LitKind),
    Keyword(Keyword),
    Ident(sym::Sym),

    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Percentage,
    Caret,
    Question,
    Tilde,
    Not,

    Lt,
    Gt,
    Ampersand,
    Pipe,

    Eq2,
    Lt2,
    Gt2,
    Ampersand2,
    Pipe2,
    Leq,
    Geq,

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
    Set,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: sym::Sym,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LitKind {
    Int(i64),
    Char(u8),
    Str(Vec<u8>),
}

#[derive(Clone, Debug)]
pub(super) struct TokenCooker<'a> {
    symtab: sym::SymbolTable,
    pos: usize,
    src: &'a str,
    errors: Vec<LError>,
}

impl<'a> TokenCooker<'a> {
    pub fn new(symtab: sym::SymbolTable, src: &'a str) -> Self {
        TokenCooker {
            symtab,
            pos: 0,
            src,
            errors: Vec::new(),
        }
    }

    pub fn consume(self) -> (sym::SymbolTable, Vec<LError>) {
        (self.symtab, self.errors)
    }

    fn err(&mut self, kind: LErrorKind, start: usize) {
        self.errors.push(LError {
            kind,
            span: Span::new(start, self.pos),
        });
    }

    pub fn next_token(&mut self) -> (Spacing, Token) {
        let mut spacing = Spacing::Joint;

        loop {
            let remaining_src = &self.src[self.pos..];
            if remaining_src.is_empty() {
                return (
                    spacing,
                    Token::new(TokKind::Eof, Span::new(self.pos, self.pos)),
                );
            }

            let start = self.pos;
            let token = raw::first_token(remaining_src);
            self.pos += token.len;

            match self.cook_token(token.kind, start) {
                Some(kind) => {
                    return (
                        spacing,
                        Token::new(kind, Span::new(start, self.pos)),
                    );
                }
                None => spacing = Spacing::Alone,
            }
        }
    }

    fn cook_token(
        &mut self,
        raw: raw::TokenKind,
        start: usize,
    ) -> Option<TokKind> {
        match raw {
            raw::TokenKind::LineComment => None,
            raw::TokenKind::Whitespace => None,
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

            raw::TokenKind::SemiColon => Some(SemiColon),
            raw::TokenKind::Comma => Some(Comma),
            raw::TokenKind::Dot => Some(Dot),

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

            raw::TokenKind::Lt => Some(Lt),
            raw::TokenKind::Gt => Some(Gt),
            raw::TokenKind::Ampersand => Some(Ampersand),
            raw::TokenKind::Pipe => Some(Pipe),

            raw::TokenKind::Eq2 => Some(Eq2),
            raw::TokenKind::Lt2 => Some(Lt2),
            raw::TokenKind::Gt2 => Some(Gt2),
            raw::TokenKind::Ampersand2 => Some(Ampersand2),
            raw::TokenKind::Pipe2 => Some(Pipe2),
            raw::TokenKind::Leq => Some(Leq),
            raw::TokenKind::Geq => Some(Geq),

            raw::TokenKind::Minus => Some(Minus),
            raw::TokenKind::Plus => Some(Plus),
            raw::TokenKind::Star => Some(Star),
            raw::TokenKind::Percentage => Some(Percentage),
            raw::TokenKind::Exclamation => Some(Not),
            raw::TokenKind::Question => Some(Question),
            raw::TokenKind::Slash => Some(Slash),
            raw::TokenKind::Caret => Some(Caret),
            raw::TokenKind::Tilde => Some(Tilde),

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
        let content =
            &mut self.src[start + 1..self.pos - 1].chars().into_iter();
        let mut bytes = Vec::new();
        loop {
            match &content.next() {
                Some('\\') => match content.next() {
                    Some('n') => bytes.push('\n' as u8),
                    Some('t') => bytes.push('\t' as u8),
                    Some('r') => bytes.push('\r' as u8),
                    Some('0') => bytes.push('\0' as u8),
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
            "set" => Keyword(Keyword::Set),
            id => Ident(self.symtab.insert(id)),
        }
    }
}

#[cfg(test)]
pub fn tokenize(src: &str) -> impl Iterator<Item = (Spacing, Token)> + '_ {
    let symtab = sym::SymbolTable::new();
    let mut cooker = TokenCooker::new(symtab, src);
    let mut empty = false;
    std::iter::from_fn(move || {
        if empty {
            None
        } else {
            let (spacing, token) = cooker.next_token();
            empty = token.kind == Eof;
            Some((spacing, token))
        }
    })
}
