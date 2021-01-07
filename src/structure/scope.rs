use std::collections::HashMap;

use super::error::{SErrorKind, SResult};
use super::eval::Val;
use super::*;
use crate::spec::ast;
use crate::SymbolTable;

pub type Namespace<'n> = HashMap<Sym, Name<'n>>;
pub type IndexSpace<'n> = Vec<Name<'n>>;

#[derive(Clone, Debug)]
pub enum Name<'n> {
    Value(Val),
    Field(Ptr),
    Struct(NameStruct<'n>),
    Array(NameArray<'n>),
    Spec(&'n ast::Struct),
    Function(NameFunction),
    Reference(&'n Name<'n>),
}

#[derive(Clone, Debug)]
pub struct NameStruct<'n> {
    pub start: u64,
    pub size: u64,
    pub fields: Namespace<'n>,
}

#[derive(Clone, Debug)]
pub struct NameArray<'n> {
    pub start: u64,
    pub size: u64,
    pub elements: IndexSpace<'n>,
}

#[derive(Clone, Debug)]
pub enum NameFunction {
    AddrOf,
    SizeOf,
    EndOf,
    Len,
}

impl<'n> Name<'n> {
    pub fn get_field(&self, sym: Sym) -> SResult<&Name<'n>> {
        if let Name::Struct(st) = self {
            st.fields
                .get(&sym)
                .ok_or(SErrorKind::IdentifierNotInScope(sym))
        } else {
            Err(SErrorKind::NotAStruct)
        }
    }

    pub fn get_element(&self, idx: usize) -> SResult<&Name<'n>> {
        if let Name::Array(arr) = self {
            arr.elements
                .get(idx)
                .ok_or(SErrorKind::IndexNotFound(idx as u64))
        } else {
            Err(SErrorKind::NotAnArray)
        }
    }

    pub fn elements(&self) -> SResult<&IndexSpace> {
        if let Name::Array(arr) = self {
            Ok(&arr.elements)
        } else {
            Err(SErrorKind::NotAnArray)
        }
    }

    pub fn start(&self) -> Option<u64> {
        match self {
            Name::Field(ptr) => Some(ptr.start),
            Name::Struct(nst) => Some(nst.start),
            Name::Array(narr) => Some(narr.start),
            _ => None,
        }
    }

    pub fn size(&self) -> Option<u64> {
        match self {
            Name::Field(ptr) => Some(ptr.pty.size() as u64),
            Name::Struct(nst) => Some(nst.size),
            Name::Array(narr) => Some(narr.size),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
struct StructSpace<'n> {
    static_scope: Namespace<'n>, // accessible to all inner structs
    local_scopes: Vec<Namespace<'n>>, // last is most inner
    blocks: Vec<NameStruct<'n>>,
}

#[derive(Clone, Debug)]
pub struct Scope<'n> {
    structs: Vec<StructSpace<'n>>, // last most inner
    self_sym: Sym,
    unnamed: Sym, // next sym for unnamed
}

impl<'n> Scope<'n> {
    pub fn new(file_length: u64, st: &mut SymbolTable) -> Self {
        let mut ns = Namespace::new();

        let self_sym = st.insert("self");
        ns.insert(st.insert("len"), Name::Function(NameFunction::Len));
        ns.insert(st.insert("addrof"), Name::Function(NameFunction::SizeOf));
        ns.insert(st.insert("sizeof"), Name::Function(NameFunction::SizeOf));
        ns.insert(st.insert("endof"), Name::Function(NameFunction::SizeOf));

        let builtins = StructSpace {
            static_scope: ns,
            local_scopes: vec![Namespace::new()],
            blocks: vec![NameStruct {
                start: 0,
                size: file_length,
                fields: Namespace::new(),
            }],
        };
        Scope {
            structs: vec![builtins],
            self_sym,
            unnamed: Sym::max_value(),
        }
    }

    pub fn get(&self, sym: Sym) -> SResult<&Name<'n>> {
        let mut name = self.get_direct(sym)?;
        while let Name::Reference(inner_name) = name {
            name = inner_name;
        }

        Ok(name)
    }

    fn get_direct(&self, sym: Sym) -> SResult<&Name<'n>> {
        let current_struct = self.structs.last().unwrap();

        /* check all local scopes within struct for variables */
        if let Some(name) = current_struct
            .local_scopes
            .iter()
            .rev()
            .flat_map(|s| s.get(&sym))
            .next()
        {
            return Ok(&name);
        }

        /* check all local blocks for previous fields */
        if let Some(name) = current_struct
            .blocks
            .iter()
            .rev()
            .flat_map(|b| b.fields.get(&sym))
            .next()
        {
            return Ok(&name)
        }

        /* check all above structs for struct specs and parameters */
        for st in self.structs.iter().rev() {
            if let Some(name) = st.static_scope.get(&sym) {
                return Ok(&name);
            }
        }

        Err(SErrorKind::IdentifierNotInScope(sym))
    }

    pub fn insert_field(&mut self, sym_opt: Option<Sym>, name: Name<'n>) {
        let sym = if let Some(sym) = sym_opt {
            sym
        } else {
            self.unnamed -= 1;
            self.unnamed
        };

        let curr = &mut self.structs.last_mut().unwrap().blocks.last_mut().unwrap();

        let start = name.start().unwrap();
        let size = name.size().unwrap();

        if curr.fields.is_empty() {
            curr.start = start;
            curr.size = size;
        } else {
            let end = start + size;
            let curr_end = curr.start + curr.size;
            curr.start = u64::min(start, curr.start);
            let next_end = u64::max(end, curr_end);
            curr.size = next_end - curr.start;
        }

        curr.fields.insert(sym, name);
    }

    pub fn enter_struct(&mut self, base: u64, static_scope: Namespace<'n>) {
        self.structs.push(StructSpace {
            static_scope,
            local_scopes: vec![Namespace::new()],
            blocks: vec![NameStruct {
                start: base,
                size: 0,
                fields: Namespace::new(),
            }],
        });
    }

    pub fn exit_struct(&mut self) -> NameStruct<'n> {
        self.structs.pop().unwrap().blocks.pop().unwrap()
    }

    pub fn enter_subblock(&mut self, base: u64) {
        self.structs.last_mut().unwrap().blocks.push(NameStruct {
            start: base,
            size: 0,
            fields: Namespace::new(),
        });
    }

    pub fn exit_subblock(&mut self) -> NameStruct<'n> {
        self.structs.last_mut().unwrap().blocks.pop().unwrap()
    }

    pub fn enter_subscope(&mut self, ns: Namespace<'n>) {
        self.structs.last_mut().unwrap().local_scopes.push(ns);
    }

    pub fn exit_subscope(&mut self) -> Namespace<'n> {
        self.structs.last_mut().unwrap().local_scopes.pop().unwrap()
    }

    pub fn enter_selfscope(&mut self, self_name: Name<'n>) {
        let mut ns = Namespace::new();
        ns.insert(self.self_sym, self_name);
        self.enter_subscope(ns);
    }

    pub fn exit_selfscope(&mut self) -> Name<'n> {
        let mut ns = self.exit_subscope();
        ns.remove(&self.self_sym).unwrap()
    }
}
