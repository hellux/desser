use crate::sym;

mod format;
mod parse;
pub mod view;
pub use parse::FileParser;

use crate::spec::ast::Order;

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
    Char,
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

impl PrimType {
    fn size(&self) -> u8 {
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
    pub fields: Vec<(Option<sym::Sym>, StructField)>,
}

#[derive(Debug)]
pub struct StructuredFile {
    pub size: u64,
    pub root: Struct,
}

impl Struct {
    pub fn last(&self) -> Option<&Ptr> {
        self.fields.last().map(|(_, f)| f.kind.last()).flatten()
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
                kinds.last().map(|k| k.last()).flatten()
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
