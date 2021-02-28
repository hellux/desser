use std::fmt;
use std::str::Chars;

use self::LiteralKind::{Char, Int, Str};
use self::Symbol::*;
use self::TokenKind::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Symbol {
    SemiColon,
    Comma,

    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Percentage,
    Caret,
    Exclamation,
    Question,
    Tilde,
    Apostrophe,

    Eq,
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
    Neq,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TokenKind {
    LineComment,
    BlockComment { closed: bool },
    Whitespace,

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,

    Literal(LiteralKind),
    Ident,
    Sym(Symbol),

    Unknown,
}

#[derive(Clone, Copy, Debug)]
pub enum LiteralKind {
    Int(Base),
    Char { closed: bool },
    Str { closed: bool },
}

#[derive(Clone, Copy, Debug)]
pub enum Base {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}

impl Base {
    pub fn radix(self) -> u32 {
        match self {
            Base::Binary => 2,
            Base::Octal => 8,
            Base::Decimal => 10,
            Base::Hexadecimal => 16,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

#[derive(Debug)]
struct Cursor<'a> {
    initial_len: usize,
    chars: Chars<'a>,
}

fn is_id_start(c: char) -> bool {
    match c {
        '_' => true,
        c if c.is_ascii_alphabetic() => true,
        _ => false,
    }
}

impl<'a> Cursor<'a> {
    fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            initial_len: input.len(),
            chars: input.chars(),
        }
    }

    /// Peek at first char without consuming it.
    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    /// Peek at nth char without consuming it.
    fn peekn(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or('\0')
    }

    /// Move to the next character.
    fn eat(&mut self) -> Option<char> {
        Some(self.chars.next()?)
    }

    /// Eat while predicate is true or until EOF.
    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.chars.as_str().is_empty() {
            self.eat();
        }
    }

    fn len_consumed(&self) -> usize {
        self.initial_len - self.chars.as_str().len()
    }
}

impl Cursor<'_> {
    fn advance_token(&mut self) -> Token {
        let kind = match self.eat().unwrap() {
            '/' => match self.peek() {
                '/' => {
                    self.eat_while(|c| c != '\n');
                    LineComment
                }
                '*' => self.block_comment(),
                _ => Sym(Slash),
            },

            '\'' => {
                if self.peek() == '\\' || self.peekn(1) == '\'' {
                    Literal(Char {
                        closed: self.eat_string('\''),
                    })
                } else {
                    Sym(Apostrophe)
                }
            }
            '"' => Literal(Str {
                closed: self.eat_string('"'),
            }),

            ' ' | '\n' => {
                self.eat_while(|c| c == ' ' || c == '\n');
                Whitespace
            }

            c if is_id_start(c) => {
                self.eat_while(|c| is_id_start(c) || c.is_digit(10));
                Ident
            }

            c @ '0'..='9' => self.number(c),

            '=' => match self.peek() {
                '=' => {
                    self.eat();
                    Sym(Eq2)
                }
                _ => Sym(Eq),
            },
            '<' => match self.peek() {
                '<' => {
                    self.eat();
                    Sym(Lt2)
                }
                '=' => {
                    self.eat();
                    Sym(Leq)
                }
                _ => Sym(Lt),
            },
            '>' => match self.peek() {
                '>' => {
                    self.eat();
                    Sym(Gt2)
                }
                '=' => {
                    self.eat();
                    Sym(Geq)
                }
                _ => Sym(Gt),
            },
            '!' => match self.peek() {
                '=' => {
                    self.eat();
                    Sym(Neq)
                }
                _ => Sym(Exclamation),
            },
            '&' => match self.peek() {
                '&' => {
                    self.eat();
                    Sym(Ampersand2)
                }
                _ => Sym(Ampersand),
            },
            '|' => match self.peek() {
                '|' => {
                    self.eat();
                    Sym(Pipe2)
                }
                _ => Sym(Pipe),
            },

            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            ';' => Sym(SemiColon),
            ',' => Sym(Comma),
            '.' => Sym(Dot),
            '?' => Sym(Question),
            '-' => Sym(Minus),
            '+' => Sym(Plus),
            '*' => Sym(Star),
            '^' => Sym(Caret),
            '~' => Sym(Tilde),
            '%' => Sym(Percentage),

            _ => Unknown,
        };

        Token {
            kind,
            len: self.len_consumed(),
        }
    }

    fn block_comment(&mut self) -> TokenKind {
        self.eat();
        let mut depth = 1;
        while depth > 0 {
            self.eat_while(|c| c != '*' && c != '/');
            match self.eat() {
                Some('/') => {
                    if self.peek() == '*' {
                        self.eat();
                        depth += 1
                    }
                }
                Some('*') => {
                    if self.peek() == '/' {
                        self.eat();
                        depth -= 1
                    }
                }
                _ => return BlockComment { closed: false },
            }
        }
        BlockComment { closed: true }
    }

    fn number(&mut self, first: char) -> TokenKind {
        let mut base = Base::Decimal;

        if first == '0' {
            match self.peek() {
                'b' => {
                    base = Base::Binary;
                    self.eat();
                    self.eat_digits(base)
                }
                'o' => {
                    base = Base::Octal;
                    self.eat();
                    self.eat_digits(base)
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.eat();
                    self.eat_digits(base)
                }
                '0'..='9' | '_' | '.' | 'e' | 'E' => self.eat_digits(base),
                _ => return Literal(Int(base)),
            };
        } else {
            self.eat_digits(base);
        };

        Literal(Int(base))
    }

    fn eat_digits(&mut self, base: Base) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => {
                    self.eat();
                }
                c if c.is_digit(base.radix()) => {
                    self.eat();
                    has_digits = true;
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_string(&mut self, delim: char) -> bool {
        self.eat();
        loop {
            self.eat_while(|c| c != delim && c != '\\');
            match self.eat() {
                Some('\\') => {
                    self.eat();
                }
                Some(c) if c == delim => return true,
                _ => return false,
            }
        }
    }
}

pub fn first_token(input: &str) -> Token {
    Cursor::new(input).advance_token()
}

#[cfg(test)]
pub fn tokenize(mut input: &str) -> impl Iterator<Item = Token> + '_ {
    std::iter::from_fn(move || {
        if input.is_empty() {
            None
        } else {
            let token = first_token(input);
            input = &input[token.len..];
            Some(token)
        }
    })
}
