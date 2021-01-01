use std::fs::File;
use std::io::{BufReader, Read};

use desser::SpecFile;

mod view {
    use std::io;
    use std::io::{BufRead, Seek, Write};

    use desser::format;
    use desser::{PrimType, Ptr, Struct, Array, StructFieldKind, SymbolTable};

    pub fn view_file<R: BufRead + Seek>(
        f: &mut R,
        st: &Struct,
        symtab: &SymbolTable,
    ) -> io::Result<String> {
        let mut s = Vec::new();
        let mut v = Viewer::new(f, &mut s, symtab);
        v.format(st)?;
        Ok(String::from_utf8_lossy(&s).to_string())
    }

    struct Viewer<'a, R: BufRead + Seek, W: Write> {
        f: &'a mut R,
        out: &'a mut W,
        symtab: &'a SymbolTable,
        addr_len: usize,
    }

    impl<'a, R: BufRead + Seek, W: Write> Viewer<'a, R, W> {
        fn new(f: &'a mut R, out: &'a mut W, symtab: &'a SymbolTable) -> Self {
            Viewer {
                f,
                out,
                symtab,
                addr_len: 5,
            }
        }

        fn format(&mut self, st: &Struct) -> io::Result<()> {
            self.addr_len = format!("{:x}", st.size).len();
            self.fmt_struct(st, 0)
        }

        fn prepend_addr(&mut self, kind: &StructFieldKind) -> io::Result<()> {
            let bit = kind.start() % 8;
            let bit_str = if bit > 0 {
                format!(":{}", bit)
            } else {
                format!("  ")
            };

            if kind.is_leaf() {
                write!(
                    self.out,
                    "{:0>l$x}{}    ",
                    kind.start() / 8,
                    bit_str,
                    l = self.addr_len
                )
            } else {
                write!(self.out, "{:l$}", "", l = self.addr_len + 6)
            }
        }

        fn fmt_struct(&mut self, st: &Struct, level: usize) -> io::Result<()> {
            if level > 0 {
                write!(self.out, "0x{:x} ", st.size / 8)?;
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
                write!(self.out, "{:l$}", "", l = self.addr_len + 6)?;
                self.out.write(&vec![b' '; 4 * (level - 1)])?;
                self.out.write(b"}")?;
            }

            Ok(())
        }

        fn fmt_array(
            &mut self,
            arr: &Array,
            level: usize,
        ) -> io::Result<()> {
            let w = format!("{}", arr.elements.len()).len();

            write!(self.out, "0x{:x} ", arr.size / 8)?;

            if !arr.elements.is_empty() {
                if let (
                    _,
                    StructFieldKind::Prim(Ptr {
                        pty: PrimType::Char,
                        ..
                    }),
                ) = arr.elements[0]
                {
                    for (_, kind) in &arr.elements {
                        self.fmt_field(kind, level)?;
                    }
                } else {
                    self.out.write(b"[\n")?;
                    for (i, kind) in &arr.elements {
                        self.prepend_addr(kind)?;
                        self.out.write(&vec![b' '; 4 * level])?;
                        write!(self.out, "{:>w$}: ", i, w = w,)?;
                        self.fmt_field(kind, level)?;
                        self.out.write(b",\n")?;
                    }

                    write!(self.out, "{:l$}", "", l = self.addr_len + 6)?;
                    self.out.write(&vec![b' '; 4 * (level - 1)])?;
                    self.out.write(b"]")?;
                }
            } else {
                self.out.write(b"[]")?;
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
                StructFieldKind::Array(arr) => {
                    self.fmt_array(arr, level + 1)
                }
                StructFieldKind::Struct(st) => self.fmt_struct(&st, level + 1),
            }
        }
    }
}

struct Options {
    spec_file: SpecFile,
    input_file: File,
}

fn exit_usage(program: &str) {
    eprintln!("usage: {} (-f specfile | 'spec') [binary_file]", program);
    std::process::exit(1);
}

fn parse_options() -> Options {
    let mut args = std::env::args();
    let program = args.next().unwrap();

    let mut spec = None;
    let mut spec_fname = None;
    let mut input_fname = None;

    while let Some(arg) = args.next().take() {
        if arg.starts_with("-f") {
            if spec.is_none() && spec_fname.is_none() {
                let sf: String = arg.chars().skip(2).collect();
                spec_fname = if sf.is_empty() {
                    args.next().take()
                } else {
                    Some(sf)
                }
            } else {
                eprintln!("may only specify one spec");
                exit_usage(&program);
            }
        } else if spec.is_none() && spec_fname.is_none() {
            spec = Some(arg);
        } else if input_fname.is_none() {
            input_fname = Some(arg);
        } else {
            eprintln!("excessive argument: {}", arg);
            exit_usage(&program);
        }
    }

    let sf = if let Some(spec) = spec {
        SpecFile::new("<cmdline>", spec)
    } else {
        if let Some(spec_fname) = spec_fname {
            let mut src = String::new();
            let path = std::path::Path::new(&spec_fname);
            let mut src_file = std::fs::File::open(path).unwrap();
            src_file.read_to_string(&mut src).expect("spec not unicode");
            SpecFile::new(&path.to_string_lossy(), src)
        } else {
            eprintln!("no spec provided");
            exit_usage(&program);
            unreachable!()
        }
    };

    let input = if let Some(fname) = input_fname {
        let f = File::open(&fname).unwrap();
        f
    } else {
        eprintln!("no input file provided");
        exit_usage(&program);
        unreachable!()
    };

    Options {
        spec_file: sf,
        input_file: input,
    }
}

fn main() -> Result<(), std::io::Error> {
    let opts = parse_options();
    let spec_res = desser::parse_spec(&opts.spec_file);

    let mut errors = Vec::new();
    match spec_res {
        Ok((spec, symtab)) => {
            let mut binary_file = BufReader::new(opts.input_file);
            match desser::parse_structure(&mut binary_file, &spec, &symtab) {
                Ok(sf) => {
                    println!(
                        "{}",
                        view::view_file(&mut binary_file, &sf.root, &symtab)?
                    );
                }
                Err(e) => {
                    errors.push(e);
                }
            };
        }
        Err(mut es) => errors.append(&mut es),
    };

    if !errors.is_empty() {
        for e in errors {
            e.display(&opts.spec_file);
        }
        std::process::exit(1);
    }

    Ok(())
}
