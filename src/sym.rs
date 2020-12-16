use std::collections::HashMap;

use crate::structure::{Ptr, Val};

pub type Sym = u16;

#[derive(Clone, Debug)]
pub struct SymbolTable {
    map: HashMap<String, Sym>,
    arr: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            map: HashMap::new(),
            arr: Vec::new(),
        }
    }

    pub fn name(&self, sym: Sym) -> &str {
        &self.arr[sym as usize]
    }

    pub fn insert(&mut self, name: &str) -> Sym {
        if let Some(sym) = self.map.get(name) {
            *sym
        } else {
            let sym = self.arr.len() as Sym;
            self.arr.push(String::from(name));
            self.map.insert(String::from(name), sym);
            sym
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Sym> {
        self.map.get(name).map(|r| *r)
    }
}

#[derive(Clone, Debug)]
pub struct Namespace(HashMap<Sym, Name>);

#[derive(Clone, Debug)]
enum Name {
    Struct(Namespace),
    Field(Variable),
}

#[derive(Clone, Debug)]
pub enum Variable {
    Direct(Val),   // in memory, e.g constant or evaluated parameter
    Indirect(Ptr), // in file
}

impl Namespace {
    pub fn new() -> Self {
        Namespace(HashMap::new())
    }

    pub fn get(&self, syms: &[Sym]) -> Option<&Variable> {
        let mut ns = &self.0;
        for i in 0..syms.len() {
            match ns.get(&syms[i]) {
                Some(Name::Struct(Namespace(ss))) => ns = &ss,
                Some(Name::Field(v)) => {
                    if i == syms.len() - 1 {
                        return Some(v);
                    } else {
                        return None;
                    }
                }
                None => return None,
            }
        }

        None
    }

    pub fn insert_value(&mut self, sym: Sym, val: Val) {
        self.0.insert(sym, Name::Field(Variable::Direct(val)));
    }

    pub fn insert_pointer(&mut self, sym: Sym, ptr: Ptr) {
        self.0.insert(sym, Name::Field(Variable::Indirect(ptr)));
    }

    pub fn insert_struct(&mut self, sym: Sym, ns: Namespace) {
        self.0.insert(sym, Name::Struct(ns));
    }
}
