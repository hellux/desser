use std::fs::File;
use std::io::{BufReader, Read, SeekFrom, Seek};

mod spec;
mod structure;
mod sym;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 3 {
        let sf = spec::error::SpecFile::new(std::path::Path::new(&args[1]))?;

        let spec_res = spec::parse_spec(&sf);
        //dbg!(&spec_res);

        if let Ok((spec, symtab)) = spec_res {
            let binary_file = File::open(&args[2])?;
            let binary_file = BufReader::new(binary_file);
            let mut fp =
                structure::FileParser::new(Box::new(binary_file), spec)
                    .unwrap();
            //dbg!(&symtab);
            println!("spec loaded");
            let root_sym = symtab.lookup("root").unwrap();
            let b = fp.parse(root_sym).unwrap();
            let (mut binary_file, _) = fp.consume();
            println!("parsed");
            //dbg!(&b);
            let mut buf = Vec::with_capacity(b.size as usize / 8);
            binary_file.seek(SeekFrom::Start(0))?;
            let n = binary_file.read_to_end(&mut buf)?;
            let cursor = std::io::Cursor::new(buf);
            let mut s = Vec::new();
            let mut v = structure::view::Viewer::new(
                cursor,
                &mut s,
                symtab,
            );
            v.fmt_struct(&b.root, 0)?;
            print!("{}", String::from_utf8_lossy(&s));
        }
    } else {
        eprintln!("required args: <spec> <binary file>")
    }

    Ok(())
}
