use crate::spec::ast;
use crate::BuiltInIdent;
use crate::SymbolTable;

use super::eval::{Partial, Val};
use super::*;

#[derive(Clone, Debug)]
pub(super) struct SymSpace<T>(pub Vec<(Option<Sym>, T)>);

#[derive(Clone, Debug)]
pub(super) struct NameFieldElem {
    pub nf: NameField,
    pub hidden: bool,
    // TODO format
}

type FieldSpace = SymSpace<NameFieldElem>;
pub(super) type IndexSpace = Vec<NameField>;

impl<T> SymSpace<T> {
    pub fn new() -> Self {
        SymSpace(Vec::new())
    }

    pub fn get(&self, sym: &Sym) -> Option<&T> {
        self.0
            .iter()
            .rev()
            .find_map(|(s, e)| if *s == Some(*sym) { Some(e) } else { None })
    }

    pub fn insert(&mut self, sym: Option<Sym>, e: T) -> bool {
        let existed = sym.map(|s| self.get(&s).is_some()).unwrap_or(false);
        self.0.push((sym, e));
        existed
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Clone, Debug)]
pub(super) struct Namespace<'n> {
    space: SymSpace<Name<'n>>,
    values: Vec<Val>,
}

impl<'n> Namespace<'n> {
    pub fn new() -> Self {
        Namespace {
            space: SymSpace::new(),
            values: Vec::new(),
        }
    }

    pub fn get(&self, sym: Sym) -> Option<&Name<'n>> {
        self.space.get(&sym)
    }

    pub fn insert(&mut self, sym: Option<Sym>, name: Name<'n>) -> bool {
        self.space.insert(sym, name)
    }

    pub fn insert_partial(&mut self, sym: Sym, part: Partial<'n>) -> bool {
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

        self.insert(Some(sym), name)
    }
}

#[derive(Copy, Clone, Debug)]
pub(super) enum Name<'n> {
    Value(&'n Val),
    Field(&'n NameField),
    Spec(&'n ast::Struct),
}

#[derive(Clone, Debug)]
pub(super) enum NameField {
    Prim(Ptr),
    Struct(NameStruct),
    Array(NameArray),
    Null(BitPos),
}
pub(super) type NameStruct = StructT<FieldSpace>;
pub(super) type NameArray = ArrayT<IndexSpace>;

impl<'n> Name<'n> {
    pub fn field(&self) -> Option<&'n NameField> {
        if let Self::Field(nf) = self {
            Some(nf)
        } else {
            None
        }
    }
}

impl NameField {
    pub fn start(&self) -> BitPos {
        match self {
            Self::Prim(ptr) => ptr.start,
            Self::Struct(nst) => nst.start,
            Self::Array(narr) => narr.start,
            Self::Null(start) => *start,
        }
    }

    pub fn size(&self) -> BitSize {
        match self {
            Self::Prim(ptr) => ptr.pty.size(),
            Self::Struct(nst) => nst.size,
            Self::Array(narr) => narr.size,
            Self::Null(_) => BitCount::new(0),
        }
    }

    fn st(&self) -> &NameStruct {
        if let Self::Struct(nst) = self {
            nst
        } else {
            panic!("not a struct")
        }
    }

    fn st_mut(&mut self) -> &mut NameStruct {
        if let Self::Struct(nst) = self {
            nst
        } else {
            panic!("not a struct")
        }
    }

    fn into_st(self) -> NameStruct {
        if let Self::Struct(nst) = self {
            nst
        } else {
            panic!("not a struct")
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
}

impl<'n> Scope<'n> {
    pub fn new(file_length: BitSize, st: &mut SymbolTable) -> Self {
        let builtins = StructScope {
            static_scope: Namespace::new(),
            local_scopes: vec![Namespace::new()],
            blocks: vec![NameField::Struct(NameStruct {
                start: BitPos::new(0),
                size: file_length,
                fields: FieldSpace::new(),
            })],
        };
        Scope {
            structs: vec![builtins],
            self_sym: st.ident_sym(BuiltInIdent::IdSelf),
            super_sym: st.ident_sym(BuiltInIdent::Super),
        }
    }

    pub fn base(&self) -> BitPos {
        self.structs.last().unwrap().blocks[0].start()
    }

    pub fn get(&self, sym: Sym) -> Option<Name<'n>> {
        let current_struct = unsafe {
            std::mem::transmute::<&StructScope, &StructScope<'n>>(
                self.structs.last().unwrap(),
            )
        };

        if sym == self.super_sym {
            return Some(Name::Field(&current_struct.blocks[0]));
        }

        /* check all local scopes within struct for variables */
        if let Some(name) = current_struct
            .local_scopes
            .iter()
            .rev()
            .find_map(|s| s.get(sym))
        {
            return Some(*name);
        }

        /* check all local blocks for previous fields */
        if let Some(elem) = current_struct
            .blocks
            .iter()
            .rev()
            .find_map(|b| b.st().fields.get(&sym))
        {
            return Some(Name::Field(&elem.nf));
        }

        /* check all above structs for struct specs and parameters */
        for st in self.structs.iter().rev() {
            if let Some(name) = st.static_scope.get(sym) {
                return Some(*name);
            }
        }

        None
    }

    pub fn insert_local(&mut self, sym: Sym, part: Partial<'n>) -> bool {
        self.structs
            .last_mut()
            .unwrap()
            .local_scopes
            .last_mut()
            .unwrap()
            .insert_partial(sym, part)
    }

    pub fn insert_field(
        &mut self,
        sym: Option<Sym>,
        nf: NameField,
        hidden: bool,
    ) -> bool {
        let curr = self
            .structs
            .last_mut()
            .unwrap()
            .blocks
            .last_mut()
            .unwrap()
            .st_mut();

        let start = nf.start();
        let size = nf.size();

        if curr.fields.is_empty() {
            curr.start = start;
            curr.size = size;
        } else {
            curr.size = curr.size + size;
        }

        let s = if hidden { None } else { sym };
        curr.fields.insert(s, NameFieldElem { nf, hidden })
    }

    pub fn enter_struct(&mut self, base: BitPos, static_scope: Namespace<'n>) {
        let blocks = vec![NameField::Struct(NameStruct {
            start: base,
            size: BitSize::new(0),
            fields: FieldSpace::new(),
        })];
        self.structs.push(StructScope {
            static_scope,
            local_scopes: vec![Namespace::new()],
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
        ns.insert(Some(self.self_sym), Name::Field(self_nf));
        self.enter_subscope(ns);
    }

    pub fn exit_selfscope(&mut self) {
        self.exit_subscope();
    }
}
