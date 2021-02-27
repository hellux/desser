use std::rc::Rc;

use crate::spec::ast;
use crate::BuiltInIdent;
use crate::SymbolTable;

use super::eval::{Partial, Val};
use super::*;

type FieldSpace = Vec<(Option<Sym>, Field)>;
pub type Namespace = Vec<(Sym, Name)>;

#[derive(Clone, Debug)]
pub enum Name {
    Value(Rc<Val>),
    Field(Rc<FieldKind>),
    Spec(Rc<ast::Struct>),
}

impl Name {
    pub fn field(&self) -> Option<Rc<FieldKind>> {
        if let Self::Field(field) = self {
            Some(field.clone())
        } else {
            None
        }
    }
}

impl From<Partial> for Name {
    fn from(part: Partial) -> Self {
        match part {
            Partial::Value(val) => Name::Value(Rc::new(val)),
            Partial::Name(name) => name,
        }
    }
}

impl FieldKind {
    fn st(&self) -> &Struct {
        if let Self::Struct(st) = self {
            st
        } else {
            panic!("not a struct")
        }
    }

    fn st_mut(&mut self) -> &mut Struct {
        if let Self::Struct(st) = self {
            st
        } else {
            panic!("not a struct")
        }
    }

    fn into_st(self) -> Struct {
        if let Self::Struct(st) = self {
            st
        } else {
            panic!("not a struct")
        }
    }
}

#[derive(Clone, Debug)]
struct StructScope {
    static_scope: Namespace, // accessible to all inner structs
    local_scopes: Vec<Namespace>, // last is most inner
    blocks: Vec<Rc<FieldKind>>,
}

#[derive(Clone, Debug)]
pub(super) struct Scope {
    structs: Vec<StructScope>, // last most inner
    self_sym: Sym,
    super_sym: Sym,
}

impl Scope {
    pub fn new(file_length: BitSize, st: &mut SymbolTable) -> Self {
        let builtins = StructScope {
            static_scope: Namespace::new(),
            local_scopes: vec![Namespace::new()],
            blocks: vec![Rc::new(FieldKind::Struct(Struct {
                start: BitPos::new(0),
                size: file_length,
                fields: FieldSpace::new(),
            }))],
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

    pub fn get(&self, sym: Sym) -> Option<Name> {
        let current_struct = self.structs.last().unwrap();

        if sym == self.super_sym {
            return Some(Name::Field(current_struct.blocks[0].clone()));
        }

        /* check all local scopes within struct for variables */
        if let Some(name) = current_struct
            .local_scopes
            .iter()
            .rev()
            .find_map(|s| s.sym_get(sym))
        {
            return Some(name.clone());
        }

        /* check all local blocks for previous fields */
        if let Some(field) = current_struct
            .blocks
            .iter()
            .rev()
            .find_map(|b| b.st().fields.sym_get(sym))
        {
            return Some(Name::Field(field.kind.clone()));
        }

        /* check all above structs for struct specs and parameters */
        for st in self.structs.iter().rev() {
            if let Some(name) = st.static_scope.sym_get(sym) {
                return Some(name.clone());
            }
        }

        None
    }

    pub fn insert_local(&mut self, sym: Sym, part: Partial) -> bool {
        self.structs
            .last_mut()
            .unwrap()
            .local_scopes
            .last_mut()
            .unwrap()
            .sym_insert(sym, part.into())
    }

    pub fn insert_field(
        &mut self,
        sym: Option<Sym>,
        kind: Rc<FieldKind>,
        hidden: bool,
    ) -> bool {
        let curr = std::rc::Rc::get_mut(
            self.structs.last_mut().unwrap().blocks.last_mut().unwrap(),
        )
        .unwrap()
        .st_mut();

        let start = kind.start();
        let size = kind.size();

        if curr.fields.is_empty() {
            curr.start = start;
            curr.size = size;
        } else {
            curr.size = curr.size + size;
        }

        let s = if hidden { None } else { sym };
        curr.fields.sym_insert(s, Field { kind, hidden })
    }

    pub fn enter_struct(&mut self, base: BitPos, static_scope: Namespace) {
        let blocks = vec![Rc::new(FieldKind::Struct(Struct {
            start: base,
            size: BitSize::new(0),
            fields: FieldSpace::new(),
        }))];

        self.structs.push(StructScope {
            static_scope,
            local_scopes: vec![Namespace::new()],
            blocks,
        });
    }

    pub fn exit_struct(&mut self) -> Struct {
        std::rc::Rc::try_unwrap(
            self.structs.pop().unwrap().blocks.pop().unwrap(),
        )
        .unwrap()
        .into_st()
    }

    pub fn enter_subblock(&mut self, base: BitPos) {
        self.structs.last_mut().unwrap().blocks.push(Rc::new(
            FieldKind::Struct(Struct {
                start: base,
                size: BitSize::new(0),
                fields: FieldSpace::new(),
            }),
        ));
    }

    pub fn exit_subblock(&mut self) -> Struct {
        std::rc::Rc::try_unwrap(
            self.structs.last_mut().unwrap().blocks.pop().unwrap(),
        )
        .unwrap()
        .into_st()
    }

    pub fn enter_subscope(&mut self, ns: Namespace) {
        self.structs.last_mut().unwrap().local_scopes.push(ns);
    }

    pub fn exit_subscope(&mut self) -> Namespace {
        self.structs.last_mut().unwrap().local_scopes.pop().unwrap()
    }

    pub fn enter_selfscope(&mut self, self_fk: Rc<FieldKind>) {
        let mut ns = Namespace::new();
        ns.sym_insert(self.self_sym, Name::Field(self_fk));
        self.enter_subscope(ns);
    }

    pub fn exit_selfscope(&mut self) {
        self.exit_subscope();
    }
}
