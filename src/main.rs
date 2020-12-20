use std::fs::File;
use std::io::BufReader;

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
            let (binary_file, _) = fp.consume();
            println!("parsed");
            //dbg!(&b);
            let mut s = Vec::new();
            let mut v = structure::view::Viewer::new(
                binary_file,
                &mut s,
                symtab,
            );
            v.fmt_struct(&b.root, 0);
            print!("{}", String::from_utf8_lossy(&s));
        }
    } else {
        eprintln!("required args: <spec> <binary file>")
    }

    Ok(())
}
