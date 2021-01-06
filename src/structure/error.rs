use crate::{Error, ErrorType, Span, Sym, SymbolTable};

#[derive(Debug)]
pub enum SErrorKind {
    // errors from spec (could potentially be checked without binary)
    StructNotInScope(Sym),
    IdentifierNotInScope(Sym),
    NonStructMemberAccess,
    NonArrayIndexAccess,
    FormalActualMismatch,
    SelfNotValid,
    InvalidType,
    NotAValue,
    NotAStruct,
    NotAnArray,
    NotAnIntegerValue,
    NotAFloatValue,
    NotAnLValue,

    // errors while reading binary
    IndexNotFound(u64),
    InvalidValue(u64),
    EndOfFile(u64),
    AddrBeforeBase(u64),
    FailedConstraint,
}

pub type SResult<T> = Result<T, SErrorKind>;

#[derive(Debug)]
pub struct SError {
    pub span: Span,
    pub pos: u64,
    pub kind: SErrorKind,
}

impl Error {
    pub fn from(s: SError, symtab: &SymbolTable) -> Self {
        let desc = match s.kind {
            SErrorKind::StructNotInScope(sym) => {
                format!("struct '{}' not in scope", symtab.name(sym).unwrap())
            }
            SErrorKind::IdentifierNotInScope(sym) => {
                format!(
                    "identifier '{}' not in scope",
                    String::from(symtab.name(sym).unwrap()),
                )
            }
            SErrorKind::InvalidValue(val) => {
                format!(
                    "value '{}' is not valid here at 0x{:x}",
                    val,
                    s.pos / 8
                )
            }
            SErrorKind::EndOfFile(size) => {
                format!(
                    "end of file reached at {:x} while parsing field at {:x}",
                    size / 8,
                    s.pos / 8
                )
            }
            SErrorKind::AddrBeforeBase(base) => {
                format!(
                    "jump to {:x} which is before current struct base at {:x}",
                    s.pos / 8,
                    base / 8
                )
            }
            SErrorKind::FailedConstraint => {
                format!("unable to match constraint at 0x{:x}", s.pos / 8,)
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
