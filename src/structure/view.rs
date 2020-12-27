use std::io;
use std::io::{BufRead, Seek, Write};

use super::format;
use crate::structure::{Struct, StructFieldKind};
use crate::sym;

pub struct Viewer<R: BufRead + Seek, W: Write> {
    f: R,
    out: W,
    symtab: sym::SymbolTable,
}

impl<R: BufRead + Seek, W: Write> Viewer<R, W> {
    pub fn new(f: R, out: W, symtab: sym::SymbolTable) -> Self {
        Viewer { f, out, symtab }
    }

    pub fn fmt_struct(&mut self, st: &Struct, level: usize) -> io::Result<()> {
        if level > 0 {
            self.out.write(b"{\n")?;
        }
        for (id_opt, f) in &st.fields {
            self.out.write(&vec![b' '; 4 * level])?;
            if let Some(id) = id_opt {
                write!(self.out, "{}: ", self.symtab.name(*id),)?;
            }
            self.fmt_field(&f.kind, level)?;
            self.out.write(b"\n")?;
        }

        if level > 0 {
            self.out.write(&vec![b' '; 4 * (level - 1)])?;
            self.out.write(b"}")?;
        }

        Ok(())
    }

    pub fn fmt_array(
        &mut self,
        kinds: &Vec<StructFieldKind>,
        level: usize,
    ) -> io::Result<()> {
        let mut i = 0;
        let w = format!("{}", kinds.len()).len();

        self.out.write(b"[\n")?;
        for kind in kinds {
            self.out.write(&vec![b' '; 4 * level])?;
            write!(self.out, "{:>w$}: ", i, w = w,)?;
            self.fmt_field(kind, level)?;
            self.out.write(b",\n")?;

            i += 1;
        }

        self.out.write(&vec![b' '; 4 * (level - 1)])?;
        self.out.write(b"]")?;

        Ok(())
    }

    fn fmt_field(
        &mut self,
        kind: &StructFieldKind,
        level: usize,
    ) -> io::Result<()> {
        match kind {
            StructFieldKind::Prim(ptr) => {
                let data = format::read_bytes(
                    ptr.start,
                    ptr.pty.size(),
                    ptr.byte_order,
                    &mut self.f,
                );
                ptr.pty.fmt(&mut self.out, data.as_slice())
            }
            StructFieldKind::Array(kinds) => self.fmt_array(&kinds, level + 1),
            StructFieldKind::Struct(st) => self.fmt_struct(&st, level + 1),
        }
    }
}
