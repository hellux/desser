use crate::spec::ast::Expr;
use crate::{Error, ErrorType, Span, SpannedSym, Sym, SymbolTable};

use super::bits::{BitPos, BitSize};
use super::eval::IntVal;

#[derive(Debug)]
pub enum SErrorKind {
    TypeNotFound(SpannedSym),
    NonType(SpannedSym),
    FormalActualMismatch(usize, usize),
    NonPositiveAlignment(IntVal),
    EndOfFile(BitSize),
    FailedConstraint(Span),
    Expr(EError),
    FieldExists(Sym),
    AddrBeforePos(BitPos),
}

#[derive(Debug, Clone)]
pub struct EError(pub Span, pub EErrorKind);

#[derive(Debug, Copy, Clone)]
pub enum EErrorKind {
    IdentifierNotFound(Sym),
    MemberNotFound(Sym),
    ElementNotFound(usize, usize),
    NotAnAttribute(Sym),
    BinaryTypeError,
    UnaryCompound,
    NonArray,
    NonField,
    NonValue,
    NonStructMemberAccess,
    NonArrayIndexAccess,
    NonArrayIterator,
    NonIntegerIndex,
    NegativeIndex(i64),
    NonIntegerSize,
    NegativeSize(i64),
}

impl Expr {
    pub fn err(&self, kind: EErrorKind) -> EError {
        EError(self.span, kind)
    }
}

pub type SResult<T> = Result<T, SErrorKind>;
pub type EResult<T> = Result<T, EError>;

impl From<EError> for SErrorKind {
    fn from(e: EError) -> Self {
        SErrorKind::Expr(e)
    }
}

#[derive(Debug)]
pub struct SError {
    pub backtrace: Vec<(Span, Option<Sym>, BitPos)>,
    pub pos: BitPos,
    pub kind: SErrorKind,
}

impl Error {
    pub fn from(mut s: SError, symtab: &SymbolTable) -> Self {
        let desc = match &s.kind {
            SErrorKind::TypeNotFound(ssym) => format!(
                "type '{}' not found",
                String::from(symtab.name(ssym.sym).unwrap()),
            ),
            SErrorKind::EndOfFile(size) => {
                format!("end of file reached at {}", size)
            }
            SErrorKind::FailedConstraint(_) => {
                "unable to match constraint".to_string()
            }
            SErrorKind::FieldExists(sym) => format!(
                "field '{}' already exists",
                String::from(symtab.name(*sym).unwrap()),
            ),
            SErrorKind::NonType(ssym) => format!(
                "'{}' is not a type",
                String::from(symtab.name(ssym.sym).unwrap())
            ),
            SErrorKind::FormalActualMismatch(formal, actual) => format!(
                "formal argument count {} does not match actual {}",
                formal, actual
            ),
            SErrorKind::NonPositiveAlignment(al) => {
                format!("alignment {} is not positive", al)
            }
            SErrorKind::AddrBeforePos(addr) => format!(
                "address {} is before current position {}",
                addr, s.pos
            ),
            SErrorKind::Expr(EError(_, kind)) => match kind {
                EErrorKind::IdentifierNotFound(sym) => format!(
                    "identifier '{}' not found",
                    String::from(symtab.name(*sym).unwrap())
                ),
                EErrorKind::MemberNotFound(sym) => format!(
                    "struct member '{}' not found",
                    String::from(symtab.name(*sym).unwrap())
                ),
                EErrorKind::ElementNotFound(i, len) => format!(
                    "index {} out of range for array with size {}",
                    i, len
                ),
                EErrorKind::NotAnAttribute(sym) => format!(
                    "'{}' is not an attribute",
                    String::from(symtab.name(*sym).unwrap())
                ),
                EErrorKind::BinaryTypeError => {
                    "invalid types for binary operator".to_string()
                }
                EErrorKind::UnaryCompound => {
                    "cannot apply unary operators on compound types"
                        .to_string()
                }
                EErrorKind::NonArray => "not an array".to_string(),
                EErrorKind::NonField => "not a field".to_string(),
                EErrorKind::NonValue => "not a value".to_string(),
                EErrorKind::NonStructMemberAccess => {
                    "cannot access members of non structs".to_string()
                }
                EErrorKind::NonArrayIndexAccess => {
                    "cannot index non arrays".to_string()
                }
                EErrorKind::NonArrayIterator => {
                    "cannot iterate over non arrays".to_string()
                }
                EErrorKind::NonIntegerIndex => {
                    "index is not integral".to_string()
                }
                EErrorKind::NegativeIndex(i) => {
                    format!("index {} is negative", i)
                }
                EErrorKind::NonIntegerSize => {
                    "size is not integral".to_string()
                }
                EErrorKind::NegativeSize(s) => {
                    format!("size {} is negative", s)
                }
            },
        };
        let hint = None;

        let span = match s.kind {
            SErrorKind::TypeNotFound(ssym) | SErrorKind::NonType(ssym) => {
                ssym.span
            }
            SErrorKind::Expr(e) => e.0,
            SErrorKind::FailedConstraint(sp) => sp,
            _ => s.backtrace.pop().unwrap().0,
        };

        Error {
            span,
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
