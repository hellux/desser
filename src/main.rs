use std::fs::File;
use std::io::BufReader;
use std::io::Read;

mod spec;
mod structured;
mod sym;

use structured::FileParser;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 3 {
        let sf = spec::error::SpecFile::new(std::path::Path::new(&args[1]))?;
        println!("{:?}, {:?}", sf, sf.line_col(222));

        let spec_res = spec::parse_spec(&sf);
        println!("{:?}", spec_res);

        if let Ok((spec, symtab)) = spec_res {
            let binary_file = File::open(&args[2])?;
            let binary_file = BufReader::new(binary_file);
            let mut fp =
                FileParser::new(Box::new(binary_file), spec, symtab).unwrap();
            let b = fp.parse();
            println!("{:?}", b);
        }
    } else {
        eprintln!("required args: <spec> <binary file>")
    }

    Ok(())
}
