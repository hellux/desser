use std::collections::HashMap;

use crate::sym;

#[derive(Clone, Debug)]
pub struct FileSpecification {
    pub structs: HashMap<sym::Sym, Struct>,
    pub constants: sym::Namespace,
}

/* Structs */

#[derive(Clone, Debug)]
pub struct Struct {
    pub parameters: Vec<sym::Sym>,
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
}

pub const U8: FieldType = FieldType::Prim(PrimType::Unsigned(Expr::Int(8)));
pub const S8: FieldType = FieldType::Prim(PrimType::Signed(Expr::Int(8)));
pub const U16: FieldType = FieldType::Prim(PrimType::Unsigned(Expr::Int(16)));
pub const S16: FieldType = FieldType::Prim(PrimType::Signed(Expr::Int(16)));
pub const U32: FieldType = FieldType::Prim(PrimType::Unsigned(Expr::Int(32)));
pub const S32: FieldType = FieldType::Prim(PrimType::Signed(Expr::Int(32)));
pub const U64: FieldType = FieldType::Prim(PrimType::Unsigned(Expr::Int(64)));
pub const S64: FieldType = FieldType::Prim(PrimType::Signed(Expr::Int(64)));
pub const U128: FieldType =
    FieldType::Prim(PrimType::Unsigned(Expr::Int(128)));
pub const S128: FieldType = FieldType::Prim(PrimType::Signed(Expr::Int(128)));
pub const F32: FieldType =
    FieldType::Prim(PrimType::Float(Expr::Int(8), Expr::Int(23)));
pub const F64: FieldType =
    FieldType::Prim(PrimType::Float(Expr::Int(11), Expr::Int(52)));

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
