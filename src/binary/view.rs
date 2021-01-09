use std::io;
use std::io::{Read, Seek, Write};

use super::*;

pub fn view_structure<R: Read + Seek>(
    f: &mut R,
    field: &StructField,
    symtab: &SymbolTable,
) -> io::Result<String> {
    let mut s = Vec::new();
    Viewer::new(f, &mut s, symtab).format(field)?;
    Ok(String::from_utf8_lossy(&s).to_string())
}

struct Viewer<'a, R: Read + Seek, W: Write> {
    f: &'a mut R,
    out: &'a mut W,
    symtab: &'a SymbolTable,
    addr_len: usize,
}

impl<'a, R: Read + Seek, W: Write> Viewer<'a, R, W> {
    fn new(f: &'a mut R, out: &'a mut W, symtab: &'a SymbolTable) -> Self {
        Viewer {
            f,
            out,
            symtab,
            addr_len: 0,
        }
    }

    fn format(&mut self, field: &StructField) -> io::Result<()> {
        self.addr_len =
            format!("{:x}", BytePos::from(field.size()).size()).len();
        self.fmt_field(field, 0)
    }

    fn prepend_addr(&mut self, kind: &StructField) -> io::Result<()> {
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
            self.prepend_addr(&f)?;
            self.out.write_all(&vec![b' '; 4 * (level - 1)])?;
            if let Some(name) = self.symtab.name(*id) {
                write!(self.out, "{}: ", name)?;
            }
            self.fmt_field(&f, level)?;
            self.out.write_all(b"\n")?;
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

        if level > 1 {
            write!(self.out, "{} ", arr.elements.len())?;
        }

        if !arr.elements.is_empty() {
            if let StructField::Prim(Ptr {
                pty: PrimType::Char,
                ..
            }) = arr.elements[0]
            {
                for f in &arr.elements {
                    self.fmt_field(f, level)?;
                }
            } else {
                if level > 1 {
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

    fn fmt_field(
        &mut self,
        kind: &StructField,
        level: usize,
    ) -> io::Result<()> {
        match kind {
            StructField::Prim(ptr) => {
                let data = format::read_bytes(
                    ptr.start,
                    ptr.pty.size(),
                    ptr.byte_order,
                    &mut self.f,
                );
                ptr.pty.fmt(&mut self.out, data.as_slice())
            }
            StructField::Array(arr) => self.fmt_array(arr, level + 1),
            StructField::Struct(st) => self.fmt_struct(&st, level + 1),
        }
    }
}
