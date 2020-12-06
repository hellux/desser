// Inspired by rustc_lexer by The Rust project developers.

use std::str::Chars;

use self::LiteralKind::*;
use self::TokenKind::*;

#[derive(Clone, Copy, Debug)]
pub enum TokenKind {
    LineComment,
    BlockComment { closed: bool },
    Whitespace,
    Ident,
    Literal(LiteralKind),
    SemiColon,
    Comma,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Dot,
    Dollar,
    Eq,
    Lt,
    Gt,
    Minus,
    And,
    Or,
    Plus,
    Star,
    Question,
    Slash,
    Caret,
    Unknown,
}

#[derive(Clone, Copy, Debug)]
pub enum LiteralKind {
    Int(Base),
    Float(Base),
    Char { closed: bool },
    Str { closed: bool },
    Invalid,
}

#[derive(Clone, Copy, Debug)]
pub enum Base {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    len: usize,
}

#[derive(Debug)]
struct Cursor<'a> {
    initial_len: usize,
    chars: Chars<'a>,
}

fn is_whitespace(c: char) -> bool {
    match c {
        '\n' | ' ' => true,
        _ => false,
    }
}

fn is_id_start(c: char) -> bool {
    match c {
        '_' => true,
        c if c.is_ascii_alphabetic() => true,
        _ => false,
    }
}

fn is_digit(c: char, base: Base) -> bool {
    let radix = match base {
        Base::Binary => 2,
        Base::Octal => 8,
        Base::Decimal => 10,
        Base::Hexadecimal => 16,
    };
    c.is_digit(radix)
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
        self.chars.clone().nth(0).unwrap_or('\0')
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
        let peek_char = self.eat().unwrap();
        let kind = match peek_char {
            '/' => match self.peek() {
                '/' => {
                    self.eat_while(|c| c != '\n');
                    LineComment
                }
                '*' => self.block_comment(),
                _ => Slash,
            },

            '\'' => Literal(Char {
                closed: self.eat_string('\''),
            }),
            '"' => Literal(Str {
                closed: self.eat_string('"'),
            }),

            c if is_whitespace(c) => {
                self.eat_while(is_whitespace);
                Whitespace
            }

            c if is_id_start(c) => {
                self.eat_while(|c| is_id_start(c) || c.is_digit(10));
                println!("{}", self.len_consumed());
                Ident
            }

            c @ '0'..='9' => self.number(c),

            ';' => SemiColon,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '?' => Question,
            '$' => Dollar,
            '=' => Eq,
            '<' => Lt,
            '>' => Gt,
            '-' => Minus,
            '&' => And,
            '|' => Or,
            '+' => Plus,
            '*' => Star,
            '^' => Caret,

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
            let has_digits = match self.peek() {
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
            if !has_digits {
                return Literal(Invalid);
            }
        } else {
            self.eat_digits(base);
        };

        match self.peek() {
            '.' => {
                self.eat();
                if is_digit(self.peek(), base) {
                    self.eat_digits(base);
                    if self.peek() == 'e' || self.peek() == 'E' {
                        return self.eat_float_exponent(base);
                    }
                }
                Literal(Float(base))
            }
            'e' | 'E' => self.eat_float_exponent(base),
            _ => Literal(Int(base)),
        }
    }

    fn eat_float_exponent(&mut self, base: Base) -> TokenKind {
        self.eat();
        if self.peek() == '-' {
            self.eat();
        }

        if !self.eat_digits(base) {
            Literal(Invalid)
        } else {
            Literal(Float(base))
        }
    }

    fn eat_digits(&mut self, base: Base) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => {
                    self.eat();
                }
                c if is_digit(c, base) => {
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

pub fn tokenize(mut input: &str) -> impl Iterator<Item = Token> + '_ {
    std::iter::from_fn(move || {
        if input.is_empty() {
            None
        } else {
            let token = Cursor::new(input).advance_token();
            input = &input[token.len..];
            Some(token)
        }
    })
}
