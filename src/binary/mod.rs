mod bits;
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
use bits::*;
use std::io::{BufRead, Seek};
pub use structure::*;
pub use view::view_structure;

pub fn parse_structure<'s, R: BufRead + Seek>(
    f: &'s mut R,
    root_spec: &'s ast::Struct,
    symtab: &mut SymbolTable,
) -> Result<Option<StructField>, Error> {
    Ok(parse::parse(f, root_spec, symtab)?.try_into().ok())
}

#[derive(Clone, Debug)]
pub struct Ptr {
    pub start: BitPos,
    pub pty: PrimType,
    pub byte_order: Order,
}

#[derive(Clone, Debug)]
struct StructT<S> {
    pub start: BitPos,
    pub size: BitSize,
    pub fields: S,
}

#[derive(Clone, Debug)]
struct ArrayT<A> {
    pub start: BitPos,
    pub size: BitSize,
    pub elements: A,
}

type PrimType = crate::PrimType<u8>;

impl PrimType {
    pub fn size(&self) -> BitSize {
        BitSize::new(match self {
            //            PrimType::Signed(len) => *len,
            //            PrimType::Unsigned(len) => *len,
            //            PrimType::Float(exponent, mantissa) => 1 + exponent + mantissa,
            PrimType::BitVec(len) => u64::from(*len),
            PrimType::U8 | PrimType::I8 | PrimType::Char => 8,
            PrimType::U16 | PrimType::I16 => 16,
            PrimType::U32 | PrimType::I32 | PrimType::F32 => 32,
            PrimType::U64 | PrimType::I64 | PrimType::F64 => 64,
        })
    }
}
