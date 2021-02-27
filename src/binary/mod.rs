use std::rc::Rc;

mod bits;
mod error;
mod eval;
pub mod format;
mod parse;
mod scope;
mod view;

use crate::spec::ast;
use crate::{Error, Order, Sym, SymbolTable};
pub use bits::*;
use std::io::{BufRead, Seek};
pub use view::view_structure;

pub fn parse_structure<'s, R: BufRead + Seek>(
    f: &'s mut R,
    root_spec: Rc<ast::Struct>,
    symtab: &mut SymbolTable,
) -> Result<Struct, Error> {
    parse::parse(f, root_spec, symtab)
}

type PrimType = crate::PrimType<u8>;

impl PrimType {
    pub fn size(&self) -> BitSize {
        BitSize::new(match self {
            PrimType::BitVec(len) => u64::from(*len),
            PrimType::U8 | PrimType::I8 | PrimType::Char => 8,
            PrimType::U16 | PrimType::I16 => 16,
            PrimType::U32 | PrimType::I32 | PrimType::F32 => 32,
            PrimType::U64 | PrimType::I64 | PrimType::F64 => 64,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Ptr {
    pub start: BitPos,
    pub pty: PrimType,
    pub order: Order,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub kind: Rc<FieldKind>,
    pub hidden: bool,
}

#[derive(Debug, Clone)]
pub enum FieldKind {
    Prim(Ptr),
    Array(Array),
    Struct(Struct),
    Null(BitPos),
}

#[derive(Debug, Clone)]
pub struct Array {
    pub start: BitPos,
    pub size: BitSize,
    pub elements: Vec<Rc<FieldKind>>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub start: BitPos,
    pub size: BitSize,
    pub fields: Vec<(Option<Sym>, Field)>,
}

impl FieldKind {
    pub fn is_leaf(&self) -> bool {
        match self {
            Self::Prim(_) => true,
            Self::Array(arr) => {
                if let Some(rc) = arr.elements.get(0) {
                    if let &FieldKind::Prim(ptr) = &rc.as_ref() {
                        matches!(ptr.pty, PrimType::Char)
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

    pub fn start(&self) -> BitPos {
        match self {
            Self::Prim(ptr) => ptr.start,
            Self::Array(arr) => arr.start,
            Self::Struct(st) => st.start,
            Self::Null(start) => *start,
        }
    }

    pub fn size(&self) -> BitSize {
        match self {
            Self::Prim(ptr) => ptr.pty.size(),
            Self::Array(arr) => arr.size,
            Self::Struct(st) => st.size,
            Self::Null(_) => BitSize::new(0),
        }
    }
}

trait SymSpace<T> {
    fn sym_get(&self, sym: Sym) -> Option<&T>;
    fn sym_insert(&mut self, sym: Sym, val: T) -> bool;
}

trait OptSymSpace<T> {
    fn sym_get(&self, sym: Sym) -> Option<&T>;
    fn sym_insert(&mut self, sym: Option<Sym>, e: T) -> bool;
}

impl<T> OptSymSpace<T> for Vec<(Option<Sym>, T)> {
    fn sym_get(&self, sym: Sym) -> Option<&T> {
        self.iter().rev().find_map(|(s, e)| {
            if *s == Some(sym) {
                Some(e)
            } else {
                None
            }
        })
    }

    fn sym_insert(&mut self, sym: Option<Sym>, e: T) -> bool {
        let existed = sym.map(|s| self.sym_get(s).is_some()).unwrap_or(false);
        self.push((sym, e));
        existed
    }
}

impl<T> SymSpace<T> for Vec<(Sym, T)> {
    fn sym_get(&self, sym: Sym) -> Option<&T> {
        self.iter().rev().find_map(
            |(s, e)| {
                if *s == sym {
                    Some(e)
                } else {
                    None
                }
            },
        )
    }

    fn sym_insert(&mut self, sym: Sym, e: T) -> bool {
        let existed = self.sym_get(sym).is_some();
        self.push((sym, e));
        existed
    }
}
