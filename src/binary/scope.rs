use std::rc::Rc;

use crate::spec::ast;
use crate::BuiltInIdent;
use crate::SymbolTable;

use super::eval::Val;
use super::*;

type FieldSpace = Vec<(Option<Sym>, Field)>;
pub type Namespace = Vec<(Sym, Name)>;

#[derive(Clone, Debug)]
pub enum Name {
    Value(Rc<Val>),
    Field(Rc<FieldKind>),
    Def(Rc<ast::Definition>),
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
struct Scope {
    static_scope: Namespace,
    local_scopes: Vec<Namespace>,
    block_scopes: Vec<Rc<FieldKind>>,
}

#[derive(Clone, Debug)]
pub(super) struct Scopes {
    scopes: Vec<Scope>, // last most inner
    self_sym: Sym,
    super_sym: Sym,
}

impl Scopes {
    pub fn new(file_length: BitSize, st: &mut SymbolTable) -> Self {
        let builtins = Scope {
            static_scope: Namespace::new(),
            local_scopes: vec![Namespace::new()],
            block_scopes: vec![Rc::new(FieldKind::Struct(Struct {
                start: BitPos::new(0),
                size: file_length,
                fields: FieldSpace::new(),
            }))],
        };
        Scopes {
            scopes: vec![builtins],
            self_sym: st.ident_sym(BuiltInIdent::IdSelf),
            super_sym: st.ident_sym(BuiltInIdent::Super),
        }
    }

    pub fn base(&self) -> BitPos {
        self.scopes.last().unwrap().block_scopes[0].start()
    }

    pub fn get(&self, sym: Sym) -> Option<Name> {
        let current_struct = self.scopes.last().unwrap();

        if sym == self.super_sym {
            return Some(Name::Field(current_struct.block_scopes[0].clone()));
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
            .block_scopes
            .iter()
            .rev()
            .find_map(|b| b.st().fields.sym_get(sym))
        {
            return Some(Name::Field(field.kind.clone()));
        }

        /* check all above scopes for struct specs and parameters */
        for st in self.scopes.iter().rev() {
            if let Some(name) = st.static_scope.sym_get(sym) {
                return Some(name.clone());
            }
        }

        None
    }

    pub fn insert_local(&mut self, sym: Sym, name: Name) -> bool {
        self.scopes
            .last_mut()
            .unwrap()
            .local_scopes
            .last_mut()
            .unwrap()
            .sym_insert(sym, name)
    }

    pub fn insert_field(
        &mut self,
        sym: Option<Sym>,
        kind: Rc<FieldKind>,
        hidden: bool,
    ) -> bool {
        let curr = std::rc::Rc::get_mut(
            self.scopes
                .last_mut()
                .unwrap()
                .block_scopes
                .last_mut()
                .unwrap(),
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

    pub fn enter_scope(&mut self, base: BitPos, static_scope: Namespace) {
        let block_scopes = vec![Rc::new(FieldKind::Struct(Struct {
            start: base,
            size: BitSize::new(0),
            fields: FieldSpace::new(),
        }))];

        self.scopes.push(Scope {
            static_scope,
            local_scopes: vec![Namespace::new()],
            block_scopes,
        });
    }

    pub fn exit_scope(&mut self) -> Struct {
        std::rc::Rc::try_unwrap(
            self.scopes.pop().unwrap().block_scopes.pop().unwrap(),
        )
        .unwrap()
        .into_st()
    }

    pub fn enter_subblock(&mut self, base: BitPos) {
        self.scopes.last_mut().unwrap().block_scopes.push(Rc::new(
            FieldKind::Struct(Struct {
                start: base,
                size: BitSize::new(0),
                fields: FieldSpace::new(),
            }),
        ));
    }

    pub fn exit_subblock(&mut self) -> Struct {
        std::rc::Rc::try_unwrap(
            self.scopes.last_mut().unwrap().block_scopes.pop().unwrap(),
        )
        .unwrap()
        .into_st()
    }

    pub fn enter_subscope(&mut self, ns: Namespace) {
        self.scopes.last_mut().unwrap().local_scopes.push(ns);
    }

    pub fn exit_subscope(&mut self) -> Namespace {
        self.scopes.last_mut().unwrap().local_scopes.pop().unwrap()
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
