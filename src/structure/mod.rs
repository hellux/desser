use crate::sym;

mod format;
mod parse;
pub mod view;
pub use parse::FileParser;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Order {
    LittleEndian,
    BigEndian,
}

pub type Val = i64;

#[derive(Clone, Debug)]
pub struct Ptr {
    pub start: u64,
    pub pty: PrimType,
    pub byte_order: Order,
}

#[derive(Copy, Clone, Debug)]
pub enum PrimType {
    Signed(u8),
    Unsigned(u8),
    Float(u8, u8), // exponent, mantissa
    BitVec(u8),
}

impl PrimType {
    fn size(&self) -> u8 {
        match self {
            PrimType::Signed(len) => *len,
            PrimType::Unsigned(len) => *len,
            PrimType::Float(exponent, mantissa) => 1 + exponent + mantissa,
            PrimType::BitVec(len) => *len,
        }
    }
}

#[derive(Debug)]
pub enum StructFieldKind {
    Prim(PrimType),
    Array(Vec<StructFieldKind>),
    Struct(Struct),
}

#[derive(Debug)]
pub struct StructField {
    pub start: u64,
    pub size: u64,
    pub kind: StructFieldKind,
}

#[derive(Debug)]
pub struct Struct {
    pub start: u64,
    pub size: u64,
    pub fields: Vec<(Option<sym::Sym>, StructField)>,
}

#[derive(Debug)]
pub struct StructuredFile {
    pub size: u64,
    pub root: Struct,
}

impl StructFieldKind {
    pub fn size(&self) -> u64 {
        match self {
            StructFieldKind::Prim(pty) => pty.size() as u64,
            StructFieldKind::Array(kinds) => {
                kinds.iter().map(|k| k.size()).sum()
            }
            StructFieldKind::Struct(st) => st.size,
        }
    }
}
