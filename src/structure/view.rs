use std::io;
use std::io::{BufRead, Seek, Write};

use super::format;
use crate::structure::{PrimType, Ptr, Struct, StructFieldKind};
use crate::sym;

pub struct Viewer<R: BufRead + Seek, W: Write> {
    f: R,
    out: W,
    symtab: sym::SymbolTable,
    addr_len: usize,
}

impl<R: BufRead + Seek, W: Write> Viewer<R, W> {
    pub fn new(f: R, out: W, symtab: sym::SymbolTable) -> Self {
        Viewer {
            f,
            out,
            symtab,
            addr_len: 5,
        }
    }

    pub fn format(&mut self, st: &Struct) -> io::Result<()> {
        let last_addr = if let Some(ptr) = st.last() {
            ptr.start / 8
        } else {
            0
        };
        self.addr_len = format!("{:x}", last_addr).len();
        self.fmt_struct(st, 0)
    }

    fn prepend_addr(&mut self, kind: &StructFieldKind) -> io::Result<()> {
        if kind.is_leaf() {
            write!(
                self.out,
                "{:0>l$x}    ",
                kind.start() / 8,
                l = self.addr_len
            )
        } else {
            write!(self.out, "{:l$}    ", "", l = self.addr_len)
        }
    }

    fn fmt_struct(&mut self, st: &Struct, level: usize) -> io::Result<()> {
        if level > 0 {
            self.out.write(b"{\n")?;
        }
        for (id_opt, f) in &st.fields {
            self.prepend_addr(&f.kind)?;
            self.out.write(&vec![b' '; 4 * level])?;
            if let Some(id) = id_opt {
                write!(self.out, "{}: ", self.symtab.name(*id),)?;
            }
            self.fmt_field(&f.kind, level)?;
            self.out.write(b"\n")?;
        }

        if level > 0 {
            write!(self.out, "{:l$}    ", "", l = self.addr_len)?;
            self.out.write(&vec![b' '; 4 * (level - 1)])?;
            self.out.write(b"}")?;
        }

        Ok(())
    }

    fn fmt_array(
        &mut self,
        kinds: &Vec<StructFieldKind>,
        level: usize,
    ) -> io::Result<()> {
        let mut i = 0;
        let w = format!("{}", kinds.len()).len();

        if !kinds.is_empty() {
            if let StructFieldKind::Prim(Ptr {
                pty: PrimType::Char,
                ..
            }) = kinds[0]
            {
                for kind in kinds {
                    self.fmt_field(kind, level)?;
                }
            } else {
                self.out.write(b"[\n")?;
                for kind in kinds {
                    self.prepend_addr(&kind)?;
                    self.out.write(&vec![b' '; 4 * level])?;
                    write!(self.out, "{:>w$}: ", i, w = w,)?;
                    self.fmt_field(kind, level)?;
                    self.out.write(b",\n")?;

                    i += 1;
                }

                write!(self.out, "{:l$}    ", "", l = self.addr_len)?;
                self.out.write(&vec![b' '; 4 * (level - 1)])?;
                self.out.write(b"]")?;
            }
        }

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
