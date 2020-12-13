pub mod ast;
pub mod error;
mod lex;
mod parse;

use crate::sym;

pub fn parse_spec(
    src: &str,
) -> Result<(ast::FileSpecification, sym::SymbolTable), ()> {
    let mut errors: Vec<error::Error> = Vec::new();
    let symtab = sym::SymbolTable::new();
    let (stream_res, symtab, mut lerr) = lex::parse_token_trees(symtab, src);
    errors.append(&mut lerr);
    match stream_res {
        Ok(stream) => {
            let (spec_res, symtab, mut perr) =
                parse::parse_file_spec(symtab, stream);
            errors.append(&mut perr);
            match spec_res {
                Ok(spec) => return Ok((spec, symtab)),
                Err(e) => errors.push(e),
            }
        }
        Err(e) => {
            errors.push(e);
        }
    }

    for e in errors {
        println!("{:?}", e)
    }

    Err(())
}
