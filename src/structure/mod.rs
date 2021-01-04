pub mod format;
mod parse;
pub use parse::parse_structure;

use crate::{Order, Sym};

type Val = i64;
#[derive(Clone, Debug)]
pub struct Ptr {
    pub start: u64,
    pub pty: PrimType,
    pub byte_order: Order,
}

type PrimType = crate::PrimType<u8>;
type SymAccess = crate::SymAccess<u64>;

impl PrimType {
    pub fn size(&self) -> u8 {
        match self {
            PrimType::Signed(len) => *len,
            PrimType::Unsigned(len) => *len,
            PrimType::Float(exponent, mantissa) => 1 + exponent + mantissa,
            PrimType::BitVec(len) => *len,
            PrimType::Char | PrimType::U8 | PrimType::S8 => 8,
            PrimType::U16 | PrimType::S16 => 16,
            PrimType::U32 | PrimType::S32 | PrimType::F32 => 32,
            PrimType::U64 | PrimType::S64 | PrimType::F64 => 64,
            PrimType::U128 | PrimType::S128 => 128,
        }
    }
}

#[derive(Debug, Clone)]
pub enum StructFieldKind {
    Prim(Ptr),
    Array(Array),
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub kind: StructFieldKind,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub start: u64,
    pub size: u64,
    pub elements: Vec<(u64, StructFieldKind)>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub start: u64,
    pub size: u64,
    pub fields: Vec<(Option<Sym>, StructField)>,
}

#[derive(Debug, Clone)]
pub struct StructuredFile {
    pub size: u64,
    pub root: Struct,
}

impl StructFieldKind {
    pub fn is_leaf(&self) -> bool {
        match self {
            StructFieldKind::Prim(_) => true,
            StructFieldKind::Array(Array { elements, .. }) => {
                if let Some(StructFieldKind::Prim(ptr)) =
                    elements.get(0).map(|(_, k)| k)
                {
                    matches!(ptr.pty, PrimType::Char)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn start(&self) -> u64 {
        match self {
            StructFieldKind::Prim(ptr) => ptr.start,
            StructFieldKind::Array(arr) => arr.start,
            StructFieldKind::Struct(st) => st.start,
        }
    }

    /*
    pub fn size(&self) -> u64 {
        match self {
            StructFieldKind::Prim(Ptr { pty, .. }) => pty.size() as u64,
            StructFieldKind::Array(kinds) => {
                kinds.iter().map(|k| k.size()).sum()
            }
            StructFieldKind::Struct(st) => st.size,
        }
    }
    */
}
