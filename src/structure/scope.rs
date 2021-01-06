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
    self_name: NameStruct<'n>,
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
            self_name: NameStruct {
                start: 0,
                size: file_length,
                fields: Namespace::new(),
            },
        };
        Scope {
            structs: vec![builtins],
            self_sym,
            unnamed: Sym::max_value(),
        }
    }

    pub fn get(&self, sym: Sym) -> SResult<&Name<'n>> {
        let mut name = self.get_shallow(sym)?;
        while let Name::Reference(inner_name) = name {
            name = inner_name;
        }

        Ok(name)
    }

    fn get_shallow(&self, sym: Sym) -> SResult<&Name<'n>> {
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

        /* check all above structs for struct specs and parameters */
        for st in self.structs.iter().rev() {
            if let Some(name) = st.static_scope.get(&sym) {
                return Ok(&name);
            }
        }

        Err(SErrorKind::IdentifierNotInScope(sym))
    }

    pub fn insert_unnamed(&mut self, name: Name<'n>) {
        self.insert(self.unnamed, name);
        self.unnamed -= 1;
    }

    pub fn insert(&mut self, sym: Sym, name: Name<'n>) {
        self.structs
            .last_mut()
            .unwrap()
            .local_scopes
            .last_mut()
            .unwrap()
            .insert(sym, name);
    }

    pub fn enter_struct(&mut self, base: u64, static_scope: Namespace<'n>) {
        self.structs.push(StructSpace {
            static_scope,
            local_scopes: vec![Namespace::new()],
            self_name: NameStruct {
                start: base,
                size: 0,
                fields: Namespace::new(),
            },
        });
    }

    pub fn exit_struct(&mut self) -> Namespace<'n> {
        self.structs.pop().unwrap().local_scopes.pop().unwrap()
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
