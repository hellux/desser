pub mod ast;
mod lex;
mod parse;

use crate::sym;

pub fn parse_spec(
    src: &str,
) -> Result<(ast::FileSpecification, sym::SymbolTable), ()> {
    let symtab = sym::SymbolTable::new();
    let (stream, symtab, errors) = lex::parse_token_trees(symtab, src);
    if errors.is_empty() {
        let (spec, symtab) = parse::parse_file_spec(symtab, stream.unwrap());
        Ok((spec.unwrap(), symtab))
    } else {
        todo!("{:?}", errors)
    }
}
