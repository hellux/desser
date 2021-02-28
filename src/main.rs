use std::fs::File;
use std::io::{BufReader, Read};

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
            } else {
                eprintln!("no flag");
                exit_usage(&program)
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

    let input = if let Some(fname) = input_fname {
        match File::open(&fname) {
            Ok(f) => Box::new(f),
            Err(e) => {
                eprintln!("error when loading input file '{}' -- {}", fname, e);
                std::process::exit(1);
            }
        }
    } else {
        eprintln!("no input file provided");
        exit_usage(&program);
        unreachable!()
    };

    Options {
        spec_file: sf,
        input_file: input,
        view,
    }
}

fn main() {
    let opts = parse_options();
    let spec_res = desser::parse_spec(&opts.spec_file);

    let mut errors = Vec::new();
    let symtab = match spec_res {
        Ok((spec, mut symtab)) => {
            let mut binary_file = BufReader::new(opts.input_file);
            eprintln!("binary parsing..");
            match desser::parse_structure(&mut binary_file, spec, &mut symtab)
            {
                Ok(root) => {
                    if opts.view {
                        eprintln!("viewing..");
                        view_structure(
                            &mut binary_file.into_inner(),
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
