use super::bits::*;
use crate::{Error, ErrorType, Span, Sym, SymbolTable};

#[derive(Debug)]
pub enum SErrorKind {
    IdentifierNotInScope(Sym),
    FormalActualMismatch,
    InvalidType,
    NotAField,
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
    pub backtrace: Vec<(Span, Option<Sym>, BitPos)>,
    pub pos: BitPos,
    pub kind: SErrorKind,
}

impl Error {
    pub fn from(mut s: SError, symtab: &SymbolTable) -> Self {
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
            span: s.backtrace.pop().unwrap().0,
            backtrace: s
                .backtrace
                .iter()
                .map(|(sp, sy, pos)| (sp.0, *sy, *pos))
                .collect(),
            desc,
            hint,
            ty: ErrorType::Structure,
        }
    }
}
