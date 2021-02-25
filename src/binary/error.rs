use crate::spec::ast::Expr;
use crate::{Error, ErrorType, Span, SpannedSym, Sym, SymbolTable};

use super::bits::*;
use super::eval::IntVal;

#[derive(Debug)]
pub enum SErrorKind {
    TypeNotFound(SpannedSym),
    NonSpec(SpannedSym),
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
    ElementNotFound(usize),
    NotAnAttribute(Sym),
    ArgumentMismatch,
    BinaryTypeError,
    UnaryCompound,
    NonArray,
    NonField,
    NonValue,
    NonStructMemberAccess,
    NonArrayIndexAccess,
    NonArrayIterator,
    NonIntegerIndex,
    NegativeIndex,
    NonIntegerSize,
    NegativeSize,
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
            k => format!("{:?}", k),
        };
        let hint = None;

        let span = match s.kind {
            SErrorKind::TypeNotFound(ssym) => ssym.span,
            SErrorKind::NonSpec(ssym) => ssym.span,
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
