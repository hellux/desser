//! The Desser lexer, inspired by rustc_lexer.
//!
//! Lexing is divided into three parts with each their own module:
//!
//!     - **raw**: Create a raw token from a string. These tokens only contain
//!     type and length, no span or literal values. All data is turned into
//!     tokens.
//!     - **cook**: Take a raw token and provide a cooked token. These contain
//!     span, value and symbol information. Comments are removed and spacing is
//!     handled separately.
//!     - **tree**: Create a stream of token trees. A token tree is either a
//!     leaf token or a substream which is the content within two delimiters.

mod cook;
mod raw;
mod tree;

pub use cook::{Delim, Keyword, Lit, LitKind, TokKind, Token};
pub use tree::{
    parse_token_trees, DelimNode, TokTree, TokenStream, TreeAndSpace,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Spacing {
    Joint,
    Alone,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span(lo as u32, hi as u32)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LErrorKind {
    UnclosedBlockComment,
    UnclosedCharLiteral,
    UnclosedStrLiteral,
    UnexpectedCloseDelim,
    UnclosedDelim,
    UnmatchedDelim,
    InvalidIntLiteral,
    InvalidCharLiteral,
    UnknownToken,
}

#[derive(Clone, Debug)]
pub struct LError {
    kind: LErrorKind,
    span: Span,
}

pub type LResult<T> = Result<T, LError>;

#[cfg(test)]
mod tests;
