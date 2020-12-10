use std::collections::HashMap;

use crate::sym;

#[derive(Clone)]
pub struct File {
    pub structs: HashMap<sym::Sym, Struct>,
    pub constants: sym::Namespace,
}

/* Structs */

#[derive(Clone)]
pub struct Struct {
    pub parameters: Vec<sym::Sym>,
    pub fields: Vec<Field>,
}

#[derive(Clone)]
pub struct Field {
    pub start: Addr,
    pub id: Option<sym::Sym>,
    pub kind: FieldKind,
    pub constraint: Option<Constraint>,
}

#[derive(Clone)]
pub enum Constraint {
    Const(Vec<u8>),
}

#[derive(Clone)]
pub enum Addr {
    Absolute(Expr),
    Relative(Expr),
}

/* Field kinds */

#[derive(Clone)]
pub enum FieldKind {
    Value(ValueType),
    Array(Box<FieldKind>, ArraySize),
    Struct(sym::Sym, Vec<Expr>),
}

#[derive(Clone)]
pub enum ArraySize {
    Exactly(Expr),
    Within(Expr, Expr),
    AtLeast(Expr),
}

#[derive(Clone, Copy, Debug)]
pub enum ValueType {
    Signed(u8),
    Unsigned(u8),
    Float(u8, u8), // exponent, mantissa
    BitVec(u8),
}

pub const U8: ValueType = ValueType::Unsigned(8);
pub const S8: ValueType = ValueType::Signed(8);
pub const U16: ValueType = ValueType::Unsigned(16);
pub const S16: ValueType = ValueType::Signed(16);
pub const U32: ValueType = ValueType::Unsigned(32);
pub const S32: ValueType = ValueType::Signed(32);
pub const U64: ValueType = ValueType::Unsigned(64);
pub const S64: ValueType = ValueType::Signed(64);
pub const U128: ValueType = ValueType::Unsigned(128);
pub const S128: ValueType = ValueType::Signed(128);
pub const F32: ValueType = ValueType::Float(8, 23);
pub const F64: ValueType = ValueType::Float(11, 52);

/* Expressions */

#[derive(Clone)]
pub enum Expr {
    Int(i64),
    Identifier(sym::Sym),
    Binary(Box<BinOp>),
    Unary(Box<UnOp>),
}

#[derive(Clone)]
pub struct BinOp {
    pub left: Expr,
    pub right: Expr,
    pub kind: BinOpKind,
}

#[derive(Clone, Copy)]
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

#[derive(Clone)]
pub struct UnOp {
    pub expr: Expr,
    pub kind: UnOpKind,
}

#[derive(Clone, Copy)]
pub enum UnOpKind {
    Neg,
}
