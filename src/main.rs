use std::fs::File;
use std::io;
use std::io::{Read};

use desser::view_structure;
use desser::SpecFile;

struct Options {
    spec_file: SpecFile,
    input_file: Box<dyn desser::SeekRead>,
    view: bool,
}

fn exit_usage(program: &str) {
    eprintln!(
        "usage: {} [-s] (-f specfile | 'spec') [binary_file]",
        program
    );
    std::process::exit(1);
}

fn parse_options() -> Options {
    let mut args = std::env::args();
    let program = args.next().unwrap();

    let mut spec = None;
    let mut spec_fname = None;
    let mut input_fname = None;
    let mut view = true;

    while let Some(arg) = args.next().take() {
        if arg.starts_with('-') {
            if arg.len() > 1 {
                for flag in arg.chars().skip(1) {
                    match flag {
                        'f' => {
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
                        }
                        's' => view = false,
                        f => {
                            eprintln!("invalid flag -- {}", f);
                            exit_usage(&program)
                        }
                    }
                }
            } else if input_fname.is_none() {
                input_fname = Some(arg);
            } else {
                eprintln!("excessive argument: {}", arg);
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
    } else if let Some(spec_fname) = spec_fname {
        let mut src = String::new();
        let path = std::path::Path::new(&spec_fname);
        let mut src_file = match File::open(path) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("error when loading spec '{}' -- {}", spec_fname, e);
                std::process::exit(1);
            }
        };
        src_file.read_to_string(&mut src).expect("spec not unicode");
        SpecFile::new(&path.to_string_lossy(), src)
    } else {
        eprintln!("no spec provided");
        exit_usage(&program);
        unreachable!()
    };

    let input: Box<dyn desser::SeekRead> = match input_fname {
        None => Box::new(SeekBuffer::new(io::stdin())),
        Some(fname) if fname == "-" => Box::new(SeekBuffer::new(io::stdin())),
        Some(fname) => match File::open(&fname) {
            Ok(f) => Box::new(io::BufReader::new(f)),
            Err(e) => {
                eprintln!("error when loading input file '{}' -- {}", fname, e);
                std::process::exit(1);
            }
        }
    };

    Options {
        spec_file: sf,
        input_file: input,
        view,
    }
}

struct SeekBuffer<R: io::Read> {
    src: R,
    exhausted: bool,
    buf: Vec<u8>,
    pos: u64,
}

impl<R: io::Read> SeekBuffer<R> {
    fn new(src: R) -> Self {
        SeekBuffer {
            src,
            exhausted: false,
            buf: Vec::new(),
            pos: 0,
        }
    }
}

impl<R: io::Read> io::Seek for SeekBuffer<R> {
    fn seek(&mut self, style: io::SeekFrom) -> io::Result<u64> {
        let (base_pos, offset) = match style {
            io::SeekFrom::Start(n) => {
                self.pos = n;
                return Ok(n);
            }
            io::SeekFrom::End(n) => {
                if !self.exhausted {
                    self.src.read_to_end(&mut self.buf)?;
                    self.exhausted = true;
                }
                (self.buf.len() as u64, n)
            }
            io::SeekFrom::Current(n) => (self.pos, n),
        };
        let new_pos = if offset >= 0 {
            base_pos.checked_add(offset as u64)
        } else {
            base_pos.checked_sub((offset.wrapping_neg()) as u64)
        };
        match new_pos {
            Some(n) => {
                self.pos = n;
                Ok(self.pos)
            }
            None => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid seek to a negative or overflowing position",
            )),
        }
    }
}

impl<R: io::Read> io::Read for SeekBuffer<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let end_max = self.pos + buf.len() as u64;
        if end_max > self.buf.len() as u64 {
            self.src.by_ref().take(buf.len() as u64).read_to_end(&mut self.buf)?;
        }

        let end = u64::min(self.buf.len() as u64, end_max);
        buf.copy_from_slice(&self.buf[self.pos as usize..end as usize]);
        let n = end - self.pos;
        self.pos = end;
        Ok(n as usize)
    }
}

fn main() {
    let opts = parse_options();
    let spec_res = desser::parse_spec(&opts.spec_file);

    let mut errors = Vec::new();
    let symtab = match spec_res {
        Ok((spec, mut symtab)) => {
            let mut binary_file = opts.input_file;
            eprintln!("binary parsing..");
            match desser::parse_structure(&mut binary_file, spec, &mut symtab)
            {
                Ok(root) => {
                    if opts.view {
                        eprintln!("viewing..");
                        view_structure(
                            &mut binary_file,
                            &mut std::io::stdout(),
                            &desser::FieldKind::Struct(root),
                            &symtab
                        ).ok();
                    }
                }
                Err(e) => {
                    errors.push(e);
                }
            };
            Some(symtab)
        }
        Err(mut es) => { errors.append(&mut es); None },
    };

    if !errors.is_empty() {
        for e in errors {
            e.display(&opts.spec_file, symtab.as_ref());
        }
        std::process::exit(1);
    }
}
