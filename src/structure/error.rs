use super::bits::*;
use crate::{Error, ErrorType, Span, Sym, SymbolTable};

#[derive(Debug)]
pub enum SErrorKind {
    IdentifierNotInScope(Sym),
    FormalActualMismatch,
    InvalidType,
    NotAStruct,
    NotAnArray,
    NotAnIntegerValue,
    NotAFloatValue,
    NegativeSize,
    IndexNotFound(u64),
    InvalidValue(u64),
    EndOfFile(BitSize),
    AddrBeforeBase(u64),
    FailedConstraint,
}

pub type SResult<T> = Result<T, SErrorKind>;

#[derive(Debug)]
pub struct SError {
    pub span: Span,
    pub pos: BitPos,
    pub kind: SErrorKind,
}

impl Error {
    pub fn from(s: SError, symtab: &SymbolTable) -> Self {
        let desc = match s.kind {
            SErrorKind::IdentifierNotInScope(sym) => format!(
                "identifier '{}' not in scope",
                String::from(symtab.name(sym).unwrap()),
            ),
            SErrorKind::InvalidValue(val) => {
                format!("value '{}' is not valid here at {}", val, s.pos)
            }
            SErrorKind::EndOfFile(size) => format!(
                "end of file reached at {} while parsing field at {}",
                size, s.pos
            ),
            SErrorKind::AddrBeforeBase(base) => format!(
                "jump to {} which is before current struct base at {:x}",
                s.pos,
                base / 8
            ),
            SErrorKind::FailedConstraint => {
                format!("unable to match constraint at {}", s.pos,)
            }
            k => format!("{:?}", k),
        };
        let hint = None;

        Error {
            span: s.span,
            desc,
            hint,
            ty: ErrorType::Structure,
        }
    }
}
