use std::io;
use std::io::Write;

use super::*;

pub fn view_structure<SR: SeekRead, W: Write>(
    f: &mut SR,
    output: &mut W,
    field: &FieldKind,
    symtab: &SymbolTable,
) -> io::Result<()> {
    let mut bufwr = std::io::BufWriter::new(output);
    Viewer::new(f, &mut bufwr, symtab).format(field)?;
    Ok(())
}

struct Viewer<'a, SR: SeekRead, W: Write> {
    f: &'a mut SR,
    out: &'a mut W,
    symtab: &'a SymbolTable,
    addr_len: usize,
}

impl<'a, SR: SeekRead, W: Write> Viewer<'a, SR, W> {
    fn new(f: &'a mut SR, out: &'a mut W, symtab: &'a SymbolTable) -> Self {
        Viewer {
            f,
            out,
            symtab,
            addr_len: 0,
        }
    }

    fn format(&mut self, kind: &FieldKind) -> io::Result<()> {
        self.addr_len =
            format!("{:x}", BytePos::from(kind.size()).size()).len();
        self.fmt_field(kind, 0)
    }

    fn prepend_addr(&mut self, kind: &FieldKind) -> io::Result<()> {
        let bit = kind.start().bit_index();
        let bit_str = if bit > 0 {
            format!(":{}", bit)
        } else {
            "  ".to_string()
        };

        if kind.is_leaf() {
            write!(
                self.out,
                "{:0>l$x}{}    ",
                BytePos::from(kind.start()).size(),
                bit_str,
                l = self.addr_len
            )
        } else {
            write!(self.out, "{:l$}", "", l = self.addr_len + 6)
        }

        /*
        if kind.is_leaf() {
            write!(self.out, "{:0>l$}{}    ", kind.start(), l = self.addr_len)
        } else {
            write!(self.out, "{:l$}", "", l = self.addr_len + 6)
        }
        */
    }

    fn fmt_struct(&mut self, st: &Struct, level: usize) -> io::Result<()> {
        if level > 1 {
            write!(self.out, "0x{:} ", st.size)?;
            self.out.write_all(b"{\n")?;
        }
        for (id, f) in &st.fields {
            if !f.hidden && !matches!(f.kind.as_ref(), FieldKind::Null(_)) {
                self.prepend_addr(&f.kind)?;
                self.out.write_all(&vec![b' '; 4 * (level - 1)])?;
                if let Some(sym) = id {
                    let name = self.symtab.name(*sym).unwrap();
                    write!(self.out, "{}: ", name)?;
                }
                self.fmt_field(&f.kind, level)?;
                self.out.write_all(b"\n")?;
            }
        }

        if level > 1 {
            write!(self.out, "{:l$}", "", l = self.addr_len + 6)?;
            self.out.write_all(&vec![b' '; 4 * (level - 2)])?;
            self.out.write_all(b"}")?;
        }

        Ok(())
    }

    fn fmt_array(&mut self, arr: &Array, level: usize) -> io::Result<()> {
        let w = format!("{}", arr.elements.len()).len();

        if !arr.elements.is_empty() {
            if let FieldKind::Prim(Ptr {
                pty: PrimType::Char,
                ..
            }) = arr.elements[0].as_ref()
            {
                for f in &arr.elements {
                    self.fmt_field(f, level)?;
                }
            } else {
                if level > 1 {
                    write!(self.out, "{} ", arr.elements.len())?;
                    self.out.write_all(b"[\n")?;
                }

                for (i, f) in arr.elements.iter().enumerate() {
                    self.prepend_addr(f)?;
                    self.out.write_all(&vec![b' '; 4 * (level - 1)])?;
                    write!(self.out, "{:0>w$}: ", i, w = w,)?;
                    self.fmt_field(f, level)?;
                    self.out.write_all(b",\n")?;
                }

                if level > 1 {
                    write!(self.out, "{:l$}", "", l = self.addr_len + 6)?;
                    self.out.write_all(&vec![b' '; 4 * (level - 2)])?;
                    self.out.write_all(b"]")?;
                }
            }
        } else {
            self.out.write_all(b"[]")?;
        }

        Ok(())
    }

    fn fmt_field(&mut self, kind: &FieldKind, level: usize) -> io::Result<()> {
        match kind {
            FieldKind::Prim(ptr) => {
                let data = ptr.read(&mut self.f);
                ptr.pty.fmt(&mut self.out, data.as_slice())
            }
            FieldKind::Array(arr) => self.fmt_array(&arr, level + 1),
            FieldKind::Struct(st) => self.fmt_struct(&st, level + 1),
            FieldKind::Null(_) => Ok(()),
        }
    }
}
