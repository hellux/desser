use crate::{AddrBase, Order, StructSpecs, Sym};

use super::Span;

/* Structs/blocks */

#[derive(Clone, Debug)]
pub struct Struct {
    pub formal_params: Vec<Sym>,
    pub structs: StructSpecs,
    pub block: Block,
}

pub type Block = Vec<Stmt>;

#[derive(Clone, Debug)]
pub enum Stmt {
    Field(Field),
    If(IfStmt),
    Constrain(Vec<Expr>),
    Debug(Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub if_body: Block,
    pub elseifs: Vec<(Expr, Block)>,
    pub else_body: Block,
}

#[derive(Clone, Debug)]
pub struct Location {
    pub expr: Option<Expr>,
    pub base: AddrBase,
    pub bitwise: bool,
}

#[derive(Clone, Debug)]
pub struct Alignment {
    pub expr: Option<Expr>,
    pub bitwise: bool,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub ty: FieldType,
    pub id: Option<Sym>,
    pub span: Span,
    pub hidden: bool,
}

/* Field types */

#[derive(Clone, Debug)]
pub struct FieldType {
    pub kind: FieldKind,
    pub byte_order: Order,
    //pub bit_order: Order,
    pub loc: Location,
    pub alignment: Alignment,
    pub constraint: Option<Expr>,
}

#[derive(Clone, Debug)]
pub enum FieldKind {
    Prim(PrimType),
    Array(Array),
    Struct(Sym, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub enum Array {
    Std(StdArray),
    For(ForArray),
}

#[derive(Clone, Debug)]
pub struct StdArray {
    pub ty: Box<FieldType>,
    pub size: ArraySize,
}

#[derive(Clone, Debug)]
pub struct ForArray {
    pub elem: Sym,
    pub arr: Vec<SymAccess>,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub enum ArraySize {
    Exactly(Expr),
    Within(Expr, Expr),
    AtLeast(Expr),
}

pub type PrimType = crate::PrimType<Expr>;
pub type SymAccess = crate::SymAccess<Expr>;

/* Expressions */

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Int(i64),
    Ident(Vec<SymAccess>),
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
    Eq,
    Neq,
    And,
    Or,
    Lt,
    Gt,
    Leq,
    Geq,
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
    Not,
}

impl BinOpKind {
    pub fn fixity(&self) -> (u8, u8) {
        match self {
            BinOpKind::Mul | BinOpKind::Div | BinOpKind::Rem => (20, 21),
            BinOpKind::Add | BinOpKind::Sub => (17, 19),
            BinOpKind::Shl | BinOpKind::Shr => (15, 16),
            BinOpKind::Lt
            | BinOpKind::Gt
            | BinOpKind::Leq
            | BinOpKind::Geq => (13, 14),
            BinOpKind::Eq | BinOpKind::Neq => (11, 12),
            BinOpKind::BitAnd => (9, 10),
            BinOpKind::BitXor => (7, 8),
            BinOpKind::BitOr => (5, 6),
            BinOpKind::And => (3, 4),
            BinOpKind::Or => (1, 2),
        }
    }
}

impl UnOpKind {
    pub fn fixity(&self) -> u8 {
        match self {
            UnOpKind::Neg | UnOpKind::Not => 22,
        }
    }
}
