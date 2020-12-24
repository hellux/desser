use std::io;
use std::io::{BufRead, Seek, Write};

use super::format;
use crate::structure::{Order, PrimType, Struct, StructFieldKind};
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
            self.fmt_field(f.start, &f.kind, level)?;
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
        mut start: u64,
        kinds: &Vec<StructFieldKind>,
        level: usize,
    ) -> io::Result<()> {
        let mut i = 0;
        let w = format!("{}", kinds.len()).len();

        self.out.write(b"[\n")?;
        for kind in kinds {
            write!(
                self.out,
                "{:indent$}{:>w$}: ",
                "",
                i,
                indent = 4 * level,
                w = w,
            )?;
            self.fmt_field(start, kind, level)?;
            self.out.write(b",\n")?;

            start += kind.size();
            i += 1;
        }

        self.out.write(&vec![b' '; 4 * (level - 1)])?;
        self.out.write(b"]")?;

        Ok(())
    }

    fn fmt_prim(
        &mut self,
        start: u64,
        pty: &PrimType,
        byte_order: Order,
    ) -> io::Result<()> {
        let data =
            format::read_bytes(start, pty.size(), byte_order, &mut self.f);
        pty.fmt(&mut self.out, data.as_slice())
    }

    fn fmt_field(
        &mut self,
        start: u64,
        kind: &StructFieldKind,
        level: usize,
    ) -> io::Result<()> {
        match kind {
            StructFieldKind::Prim(pty) => {
                self.fmt_prim(start, pty, Order::LittleEndian)
            }
            StructFieldKind::Array(kinds) => {
                self.fmt_array(start, &kinds, level + 1)
            }
            StructFieldKind::Struct(st) => self.fmt_struct(&st, level + 1),
        }
    }
}
