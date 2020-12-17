use super::format;
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
            inner.push_str(&format!("{:indent$}", "", indent = 4 * level));
            if let Some(id) = id_opt {
                inner.push_str(&format!("{}: ", self.symtab.name(*id),));
            }
            inner.push_str(&self.fmt_field(f.start, &f.kind, level));
            inner.push('\n');
        }

        format!("{{\n{}{:indent$}}}", inner, "", indent = 4 * (level - 1))
    }

    pub fn fmt_array(
        &mut self,
        mut start: u64,
        kinds: &Vec<StructFieldKind>,
        level: usize,
    ) -> String {
        let mut inner = String::new();
        let mut i = 0;
        for kind in kinds {
            inner.push_str(&format!(
                "{:indent$}{}: ",
                "",
                i,
                indent = 4 * level
            ));
            inner.push_str(&self.fmt_field(start, kind, level));
            inner.push_str(",\n");

            start += kind.size();
            i += 1;
        }

        format!("[\n{}{:indent$}]", inner, "", indent = 4 * (level - 1))
    }

    fn fmt_prim(&mut self, start: u64, ) {

    }

    fn fmt_field(
        &mut self,
        start: u64,
        kind: &StructFieldKind,
        level: usize,
    ) -> String {
        match kind {
            StructFieldKind::Prim(pty) => {
                let val =
                    format::eval(start, pty, Order::LittleEndian, &mut self.f);
                format!("{}", val)
            }
            StructFieldKind::Array(kinds) => {
                self.fmt_array(start, &kinds, level + 1)
            }
            StructFieldKind::Struct(st) => self.fmt_struct(&st, level + 1),
        }
    }
}
