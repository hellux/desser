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

use crate::{Error, ErrorType};

use super::Span;
pub use cook::{Attr, Delim, Keyword, Lit, LitKind, TokKind, Token};
pub use tree::{parse_token_trees, DelimNode, TokTree, TokenStream};

use self::LErrorKind::*;

#[derive(Clone, Copy, Debug)]
enum LErrorKind {
    UnclosedBlockComment,
    UnclosedCharLiteral,
    UnclosedStrLiteral,
    UnclosedDelim,
    UnexpectedCloseDelim,
    UnmatchedDelim,
    InvalidIntLiteral,
    InvalidCharLiteral,
    UnknownToken,
}

#[derive(Clone, Debug)]
struct LError {
    kind: LErrorKind,
    span: Span,
}

type LResult<T> = Result<T, LError>;

impl From<LError> for Error {
    fn from(l: LError) -> Self {
        let desc = match l.kind {
            UnclosedBlockComment => format!(
                "block comment reached end of file without being closed"
            ),
            UnclosedCharLiteral => format!("char literal was never closed"),
            UnclosedStrLiteral => format!("string literal was never closed"),
            UnexpectedCloseDelim => {
                format!("this closing delimiter was not expected")
            }
            UnclosedDelim => {
                format!("reached end of file without closing a delimiter")
            }
            UnmatchedDelim => {
                format!("closing delimiter does not match the latest opened")
            }
            InvalidIntLiteral => format!("integer literal is invalid"),
            InvalidCharLiteral => format!("char literal is invalid"),
            UnknownToken => format!("unrecognized token"),
        };
        let hint = None;

        Error {
            span: l.span,
            desc,
            hint,
            ty: ErrorType::Lexical,
        }
    }
}

#[cfg(test)]
mod tests;
