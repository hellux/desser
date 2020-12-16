use crate::structure::{
    Order, PrimType, Ptr, Struct, StructField, StructFieldKind,
};
use crate::sym;

use std::io::{BufRead, Seek};

pub struct Viewer<R: BufRead + Seek> {
    f: R,
    symtab: sym::SymbolTable,
}

impl<R: BufRead + Seek> Viewer<R> {
    pub fn new(f: R, symtab: sym::SymbolTable) -> Self {
        Viewer { f, symtab }
    }

    pub fn fmt_struct(&mut self, st: &Struct, level: usize) -> String {
        let mut inner = String::new();

        for (id_opt, f) in &st.fields {
            if let Some(id) = id_opt {
                inner.push_str(&format!(
                    "{:indent$}{}: ",
                    "",
                    self.symtab.name(*id),
                    indent = 4 * level
                ));
            }
            inner.push_str(&self.fmt_field(f, level));
            inner.push('\n');
        }

        format!("{{\n{}{:indent$}}}", inner, "", indent = 4 * (level - 1))
    }

    fn fmt_field(&mut self, field: &StructField, level: usize) -> String {
        match &field.kind {
            StructFieldKind::Prim(pty) => {
                let ptr = Ptr {
                    start: field.start,
                    pty: *pty,
                    byte_order: Order::LittleEndian,
                };
                let val = ptr.eval(&mut self.f);
                format!("{}", val)
            }
            StructFieldKind::Array(kinds) => format!(""),
            StructFieldKind::Struct(st) => self.fmt_struct(&st, level + 1),
        }
    }
}
