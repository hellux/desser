use std::collections::HashMap;

use crate::sym;

/* Structs */

#[derive(Clone, Debug)]
pub struct Struct {
    pub parameters: Vec<sym::Sym>,
    pub structs: HashMap<sym::Sym, Struct>,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub ty: FieldType,
    pub id: Option<sym::Sym>,
    pub constraint: Option<Constraint>,
}

#[derive(Clone, Debug)]
pub enum Constraint {
    Const(Vec<u8>),
}

/* Field types */

#[derive(Clone, Debug)]
pub enum FieldType {
    Prim(PrimType),
    Array(Box<FieldType>, ArraySize),
    Struct(sym::Sym, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub enum ArraySize {
    Exactly(Expr),
    Within(Expr, Expr),
    AtLeast(Expr),
}

#[derive(Clone, Debug)]
pub enum PrimType {
    Signed(Expr),
    Unsigned(Expr),
    Float(Expr, Expr), // exponent, mantissa
    BitVec(Expr),
    U8,
    S8,
    U16,
    S16,
    U32,
    S32,
    U64,
    S64,
    U128,
    S128,
    F32,
    F64,
}

/* Expressions */

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i64),
    Ident(Vec<sym::Sym>),
    Binary(Box<BinOp>),
    Unary(Box<UnOp>),
}

#[derive(Clone, Debug)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitXor,
    BitOr,
    Shl,
    Shr,
}

#[derive(Clone, Debug)]
pub struct UnOp {
    pub expr: Expr,
    pub kind: UnOpKind,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOpKind {
    Neg,
}

impl BinOpKind {
    pub fn fixity(&self) -> (u8, u8) {
        match self {
            BinOpKind::Mul | BinOpKind::Div | BinOpKind::Rem => (11, 12),
            BinOpKind::Add | BinOpKind::Sub => (9, 10),
            BinOpKind::Shl | BinOpKind::Shr => (7, 8),
            BinOpKind::BitAnd => (5, 6),
            BinOpKind::BitXor => (3, 4),
            BinOpKind::BitOr => (1, 2),
        }
    }
}

impl UnOpKind {
    pub fn fixity(&self) -> u8 {
        match self {
            UnOpKind::Neg => 13,
        }
    }
}
