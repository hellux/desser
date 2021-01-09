use std::collections::HashMap;

use crate::spec::ast;
use crate::BuiltIn::*;
use crate::SymbolTable;

use super::error::{SErrorKind, SResult};
use super::eval::{Partial, Val};
use super::*;

pub(super) type FieldSpace = HashMap<Sym, NameField>;
pub(super) type IndexSpace = Vec<NameField>;

#[derive(Clone, Debug)]
pub(super) struct Namespace<'n> {
    space: HashMap<Sym, Name<'n>>,
    values: Vec<Val>,
}

impl<'n> Namespace<'n> {
    pub fn new() -> Self {
        Namespace {
            space: HashMap::new(),
            values: Vec::new(),
        }
    }

    pub fn get(&self, sym: &Sym) -> Option<&Name<'n>> {
        self.space.get(sym)
    }

    pub fn insert(&mut self, sym: Sym, name: Name<'n>) {
        self.space.insert(sym, name);
    }

    pub fn insert_partial(&mut self, sym: Sym, part: Partial<'n>) {
        let name = match part {
            Partial::Value(val) => {
                self.values.push(val);
                Name::Value(unsafe {
                    std::mem::transmute::<&Val, &'n Val>(
                        self.values.last().unwrap(),
                    )
                })
            }
            Partial::Name(name) => name,
        };

        self.insert(sym, name);
    }
}

#[derive(Copy, Clone, Debug)]
pub(super) enum Name<'n> {
    Value(&'n Val),
    Field(&'n NameField),
    Spec(&'n ast::Struct),
    Func(NameFunc),
}

#[derive(Clone, Debug)]
pub(super) enum NameField {
    Prim(Ptr),
    Struct(NameStruct),
    Array(NameArray),
}
pub(super) type NameStruct = StructT<FieldSpace>;
pub(super) type NameArray = ArrayT<IndexSpace>;

#[derive(Copy, Clone, Debug)]
pub enum NameFunc {
    AddrOf,
    SizeOf,
    EndOf,
    Len,
}

impl<'n> Name<'n> {
    pub fn field(&self) -> SResult<&'n NameField> {
        if let Self::Field(nf) = self {
            Ok(nf)
        } else {
            Err(SErrorKind::NotAField)
        }
    }
}

impl NameField {
    pub fn get_field(&self, sym: Sym) -> SResult<&NameField> {
        if let Self::Struct(st) = self {
            st.fields
                .get(&sym)
                .ok_or(SErrorKind::IdentifierNotInScope(sym))
        } else {
            Err(SErrorKind::NotAStruct)
        }
    }

    pub fn get_element(&self, idx: usize) -> SResult<&NameField> {
        if let Self::Array(arr) = self {
            arr.elements
                .get(idx)
                .ok_or(SErrorKind::IndexNotFound(idx as u64))
        } else {
            Err(SErrorKind::NotAnArray)
        }
    }

    pub fn elements(&self) -> SResult<&IndexSpace> {
        if let Self::Array(arr) = self {
            Ok(&arr.elements)
        } else {
            Err(SErrorKind::NotAnArray)
        }
    }

    pub fn start(&self) -> Option<BitPos> {
        match self {
            Self::Prim(ptr) => Some(ptr.start),
            Self::Struct(nst) => Some(nst.start),
            Self::Array(narr) => Some(narr.start),
        }
    }

    pub fn size(&self) -> Option<BitSize> {
        match self {
            Self::Prim(ptr) => Some(ptr.pty.size()),
            Self::Struct(nst) => Some(nst.size),
            Self::Array(narr) => Some(narr.size),
        }
    }

    fn st(&self) -> &NameStruct {
        if let Self::Struct(nst) = self {
            nst
        } else {
            panic!()
        }
    }

    fn st_mut(&mut self) -> &mut NameStruct {
        if let Self::Struct(nst) = self {
            nst
        } else {
            panic!()
        }
    }

    fn into_st(self) -> NameStruct {
        if let Self::Struct(nst) = self {
            nst
        } else {
            panic!()
        }
    }
}

#[derive(Clone, Debug)]
struct StructScope<'n> {
    static_scope: Namespace<'n>, // accessible to all inner structs
    local_scopes: Vec<Namespace<'n>>, // last is most inner
    blocks: Vec<NameField>,
}

#[derive(Clone, Debug)]
pub(super) struct Scope<'n> {
    structs: Vec<StructScope<'n>>, // last most inner
    self_sym: Sym,
    super_sym: Sym,
    unnamed: Sym, // next sym for unnamed
}

impl<'n> Scope<'n> {
    pub fn new(file_length: BitSize, st: &mut SymbolTable) -> Self {
        let mut ns = Namespace::new();
        ns.insert(st.builtin(FuncLen), Name::Func(NameFunc::Len));
        ns.insert(st.builtin(FuncAddrOf), Name::Func(NameFunc::AddrOf));
        ns.insert(st.builtin(FuncSizeOf), Name::Func(NameFunc::SizeOf));
        ns.insert(st.builtin(FuncEndOf), Name::Func(NameFunc::EndOf));

        let builtins = StructScope {
            static_scope: ns,
            local_scopes: vec![Namespace::new()],
            blocks: vec![NameField::Struct(NameStruct {
                start: BitPos::new(0),
                size: file_length,
                fields: FieldSpace::new(),
            })],
        };
        Scope {
            structs: vec![builtins],
            self_sym: st.builtin(IdentSelf),
            super_sym: st.builtin(IdentSuper),
            unnamed: Sym::max_value(),
        }
    }

    pub fn get(&self, sym: Sym) -> SResult<Name<'n>> {
        let current_struct = unsafe {
            std::mem::transmute::<&StructScope, &StructScope<'n>>(
                self.structs.last().unwrap(),
            )
        };

        /* check all local scopes within struct for variables */
        if let Some(name) = current_struct
            .local_scopes
            .iter()
            .rev()
            .find_map(|s| s.get(&sym))
        {
            return Ok(*name);
        }

        /* check all local blocks for previous fields */
        if let Some(name) = current_struct
            .blocks
            .iter()
            .rev()
            .find_map(|b| b.st().fields.get(&sym))
        {
            return Ok(Name::Field(name));
        }

        /* check all above structs for struct specs and parameters */
        for st in self.structs.iter().rev() {
            if let Some(name) = st.static_scope.get(&sym) {
                return Ok(*name);
            }
        }

        Err(SErrorKind::IdentifierNotInScope(sym))
    }

    pub fn insert_local(&mut self, sym: Sym, part: Partial<'n>) {
        self.structs
            .last_mut()
            .unwrap()
            .local_scopes
            .last_mut()
            .unwrap()
            .insert_partial(sym, part);
    }

    pub fn insert_field(&mut self, sym_opt: Option<Sym>, nf: NameField) {
        let sym = if let Some(sym) = sym_opt {
            sym
        } else {
            self.unnamed -= 1;
            self.unnamed
        };

        let curr = &mut self
            .structs
            .last_mut()
            .unwrap()
            .blocks
            .last_mut()
            .unwrap()
            .st_mut();

        let start = nf.start().unwrap();
        let size = nf.size().unwrap();

        if curr.fields.is_empty() {
            curr.start = start;
            curr.size = size;
        } else {
            let end = start + size;
            let curr_end = curr.start + curr.size;
            curr.start = BitPos::min(start, curr.start);
            let next_end = BitPos::max(end, curr_end);
            curr.size = next_end - curr.start;
        }

        curr.fields.insert(sym, nf);
    }

    pub fn enter_struct(&mut self, base: BitPos, static_scope: Namespace<'n>) {
        let blocks = vec![NameField::Struct(NameStruct {
            start: base,
            size: BitSize::new(0),
            fields: FieldSpace::new(),
        })];
        let mut local = Namespace::new();
        local.insert(
            self.super_sym,
            Name::Field(unsafe {
                std::mem::transmute::<&NameField, &'n NameField>(&blocks[0])
            }),
        );
        self.structs.push(StructScope {
            static_scope,
            local_scopes: vec![local],
            blocks,
        });
    }

    pub fn exit_struct(&mut self) -> NameStruct {
        self.structs.pop().unwrap().blocks.pop().unwrap().into_st()
    }

    pub fn enter_subblock(&mut self, base: BitPos) {
        self.structs
            .last_mut()
            .unwrap()
            .blocks
            .push(NameField::Struct(NameStruct {
                start: base,
                size: BitSize::new(0),
                fields: FieldSpace::new(),
            }));
    }

    pub fn exit_subblock(&mut self) -> NameStruct {
        self.structs
            .last_mut()
            .unwrap()
            .blocks
            .pop()
            .unwrap()
            .into_st()
    }

    pub fn enter_subscope(&mut self, ns: Namespace<'n>) {
        self.structs.last_mut().unwrap().local_scopes.push(ns);
    }

    pub fn exit_subscope(&mut self) -> Namespace<'n> {
        self.structs.last_mut().unwrap().local_scopes.pop().unwrap()
    }

    pub fn enter_selfscope(&mut self, self_nf: &'n NameField) {
        let mut ns = Namespace::new();
        ns.insert(self.self_sym, Name::Field(self_nf));
        self.enter_subscope(ns);
    }

    pub fn exit_selfscope(&mut self) {
        self.exit_subscope();
    }
}