use std::rc::Rc;

use crate::{AddrBase, Order, SpannedSym, Sym};

use super::lex::LitKind;
use super::Span;

/* Structs/blocks */

#[derive(Clone, Debug)]
pub struct Struct {
    pub header: Header,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Header {
    pub defs: Vec<(Sym, Rc<Definition>)>,
    pub constants: Vec<(Sym, Expr)>,
}

pub type Block = Vec<Stmt>;

#[derive(Clone, Debug)]
pub struct Definition {
    pub formal_params: Vec<Sym>,
    pub ty: FieldType,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Field(Field),
    Let(Sym, Expr),
    Constrain(Vec<Expr>),
    Debug(Vec<Expr>),
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
    pub properties: Properties,
    pub kind: FieldKind,
}

#[derive(Clone, Debug)]
pub struct Properties {
    pub order: Order,
    pub loc: Location,
    pub alignment: Alignment,
    pub constraints: Vec<Constraint>,
    pub fin: Option<Box<Expr>>,
    pub peek: bool,
}

#[derive(Clone, Debug)]
pub enum Constraint {
    Generic(Expr),
    Binary(BinOp, Expr),
    Zero(bool),
}

#[derive(Clone, Debug)]
pub enum FieldKind {
    Prim(PrimType),
    Array(Array),
    Name(SpannedSym, Vec<Expr>),
    Block(Block),
    Struct(Struct),
    If(IfCase<FieldType>),
    Null,
}

pub type PrimType = crate::PrimType<Expr>;

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
    pub arr: Expr,
    pub ty: Box<FieldType>,
}

#[derive(Clone, Debug)]
pub enum ArraySize {
    Exactly(Expr),
    Within(Expr, Expr),
    AtLeast(Expr),
}

#[derive(Clone, Debug)]
pub struct IfCase<T> {
    pub cond: Box<Expr>,
    pub if_res: Box<T>,
    pub else_res: Box<T>,
}

impl FieldType {
    pub fn null() -> Self {
        FieldType {
            kind: FieldKind::Null,
            properties: Properties::default(),
        }
    }
}

impl Default for Properties {
    fn default() -> Self {
        Properties {
            order: Order::LittleEndian,
            loc: Location {
                expr: None,
                base: AddrBase::Absolute,
                bitwise: false,
            },
            alignment: Alignment {
                expr: None,
                bitwise: false,
            },
            constraints: Vec::new(),
            fin: None,
            peek: false,
        }
    }
}

/* Expressions */

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Variable(Sym),
    Member(Box<Expr>, SpannedSym),
    Index(Box<Expr>, Box<Expr>),

    Literal(LitKind),
    Attr(Box<Expr>, SpannedSym),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),

    If(IfCase<Expr>),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
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

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Neg,
    Not,
}

impl BinOp {
    pub fn fixity(self) -> (u8, u8) {
        match self {
            BinOp::Mul | BinOp::Div | BinOp::Rem => (20, 21),
            BinOp::Add | BinOp::Sub => (17, 19),
            BinOp::Shl | BinOp::Shr => (15, 16),
            BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq => (13, 14),
            BinOp::Eq | BinOp::Neq => (11, 12),
            BinOp::BitAnd => (9, 10),
            BinOp::BitXor => (7, 8),
            BinOp::BitOr => (5, 6),
            BinOp::And => (3, 4),
            BinOp::Or => (1, 2),
        }
    }
}

impl UnOp {
    pub fn fixity(self) -> u8 {
        match self {
            UnOp::Neg | UnOp::Not => 22,
        }
    }
}

impl Expr {
    pub fn int(val: i64, span: Span) -> Self {
        Expr {
            kind: ExprKind::Literal(LitKind::Int(val)),
            span,
        }
    }
}
