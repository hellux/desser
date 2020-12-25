use std::fs::File;
use std::io::{BufReader, Read, Seek, SeekFrom};

mod spec;
mod structure;
mod sym;

struct Options {
    spec_file: spec::SpecFile,
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
        spec::SpecFile::new("<cmdline>", spec)
    } else {
        if let Some(spec_fname) = spec_fname {
            let mut src = String::new();
            let path = std::path::Path::new(&spec_fname);
            let mut src_file = std::fs::File::open(path).unwrap();
            src_file.read_to_string(&mut src).expect("spec not unicode");
            spec::SpecFile::new(&path.to_string_lossy(), src)
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

    let spec_res = spec::parse_spec(&opts.spec_file);

    if let Ok((spec, symtab)) = spec_res {
        let binary_file = BufReader::new(opts.input_file);
        let mut fp = structure::FileParser::new(binary_file).unwrap();
        //dbg!(&symtab);
        println!("spec loaded");
        let b = fp.parse(&spec).unwrap();
        let binary_file = fp.consume();
        println!("parsed");
        //dbg!(&b);
        let mut s = Vec::new();
        let mut v = structure::view::Viewer::new(binary_file, &mut s, symtab);
        v.fmt_struct(&b.root, 0)?;
        print!("{}", String::from_utf8_lossy(&s));
    }

    Ok(())
}
