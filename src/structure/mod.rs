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

#[derive(Debug)]
pub enum StructFieldKind {
    Prim(Ptr),
    Array(Vec<StructFieldKind>),
    Struct(Struct),
}

#[derive(Debug)]
pub struct StructField {
    pub kind: StructFieldKind,
}

#[derive(Debug)]
pub struct Struct {
    pub size: u64,
    pub fields: Vec<(Option<Sym>, StructField)>,
}

#[derive(Debug)]
pub struct StructuredFile {
    pub size: u64,
    pub root: Struct,
}

impl Struct {
    pub fn last(&self) -> Option<&Ptr> {
        self.fields.last().and_then(|(_, f)| f.kind.last())
    }
}

impl StructFieldKind {
    pub fn is_leaf(&self) -> bool {
        match self {
            StructFieldKind::Prim(_) => true,
            StructFieldKind::Array(kinds) => {
                if let StructFieldKind::Prim(ptr) = &kinds[0] {
                    if let PrimType::Char = ptr.pty {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn last(&self) -> Option<&Ptr> {
        match self {
            StructFieldKind::Prim(ptr) => Some(ptr),
            StructFieldKind::Array(kinds) => {
                kinds.last().and_then(|k| k.last())
            }
            StructFieldKind::Struct(st) => st.last(),
        }
    }

    pub fn start(&self) -> u64 {
        match self {
            StructFieldKind::Prim(ptr) => ptr.start,
            StructFieldKind::Array(kinds) => kinds[0].start(),
            StructFieldKind::Struct(st) => st.fields[0].1.kind.start(),
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
