mod error;
mod eval;
pub mod format;
mod parse;
mod scope;
mod structure;
mod view;

use std::convert::TryInto;

use crate::spec::ast;
use crate::{Error, Order, Sym, SymbolTable};
use std::io::{BufRead, Seek};
pub use structure::*;
pub use view::view_structure;

pub fn parse_structure<'s, R: BufRead + Seek>(
    f: &'s mut R,
    root_spec: &'s ast::Struct,
    symtab: &mut SymbolTable,
) -> Result<StructField, Error> {
    Ok(parse::parse(f, root_spec, symtab)?.try_into().unwrap())
}

#[derive(Clone, Debug)]
pub struct Ptr {
    pub start: u64,
    pub pty: PrimType,
    pub byte_order: Order,
}

type PrimType = crate::PrimType<u8>;

#[derive(Copy, Clone, Debug)]
pub enum PrimKind {
    BitVec,
    Char,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    F32,
    F64,
}

impl PrimType {
    pub fn size(&self) -> u8 {
        match self {
            //            PrimType::Signed(len) => *len,
            //            PrimType::Unsigned(len) => *len,
            //            PrimType::Float(exponent, mantissa) => 1 + exponent + mantissa,
            PrimType::BitVec(len) => *len,
            PrimType::U8 | PrimType::I8 | PrimType::Char => 8,
            PrimType::U16 | PrimType::I16 => 16,
            PrimType::U32 | PrimType::I32 | PrimType::F32 => 32,
            PrimType::U64 | PrimType::I64 | PrimType::F64 => 64,
        }
    }
}
