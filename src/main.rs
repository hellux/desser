use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

mod ast;
mod lex;
mod parse;
mod structured;
mod sym;

use structured::FileParser;

#[derive(Debug)]
pub enum PError {
    EndOfFile,
    UnfulfilledConstraint,
    ExcessData,
    InvalidType,
    VariableNotInScope,
    IO(std::io::Error),
}

impl From<std::io::Error> for PError {
    fn from(e: std::io::Error) -> Self {
        PError::IO(e)
    }
}

impl From<std::io::ErrorKind> for PError {
    fn from(kind: std::io::ErrorKind) -> Self {
        PError::from(std::io::Error::from(kind))
    }
}

pub type PResult<T> = Result<T, PError>;

fn main() -> Result<(), std::io::Error> {
    /*
    let psf2: ast::File = ast::File {
        structs: vec![(
            ast::Id::from("root"),
            ast::Struct {
                parameters: vec![],
                fields: vec![
                    ast::Field {
                        start: ast::Addr::Relative(ast::Expr::Int(0)),
                        id: Some(String::from("magic")),
                        kind: ast::FieldKind::Array(
                            Box::new(ast::FieldKind::Value(ast::U8)),
                            ast::ArraySize::Exactly(ast::Expr::Binary(
                                Box::new(ast::BinOp {
                                    left: ast::Expr::Int(5),
                                    right: ast::Expr::Int(1),
                                    kind: ast::BinOpKind::Sub,
                                }),
                            )),
                        ),
                        constraint: Some(ast::Constraint::Const(vec![
                            0x72, 0xb5, 0x4a, 0x86,
                        ])),
                    },
                    ast::Field {
                        start: ast::Addr::Relative(ast::Expr::Int(0)),
                        id: Some(String::from("version")),
                        kind: ast::FieldKind::Value(ast::U32),
                        constraint: Some(ast::Constraint::Const(vec![0])),
                    },
                    ast::Field {
                        start: ast::Addr::Relative(ast::Expr::Int(0)),
                        id: Some(String::from("headersize")),
                        kind: ast::FieldKind::Value(ast::U32),
                        constraint: None,
                    },
                ],
            },
        )]
        .into_iter()
        .collect(),
        constants: HashMap::new(),
    };

    let f = File::open("font.psf").unwrap();
    let f = BufReader::new(f);
    let mut fp = FileParser::new(Box::new(f), psf2).unwrap();
    let b = fp.parse();
    println!("{:?}", b);
    */

    let src = "def root { u32 $version; bitvector(5) $vec; } ";
    let symtab = sym::SymbolTable::new();
    let (stream, symtab, errors) = lex::parse_token_trees(symtab, src);

    println!("{:?}, \nerrors: {:?}", symtab, errors);
    println!("{:?}", stream);

    let (spec, symtab) = parse::parse_file_spec(symtab, stream.unwrap());
    println!("SPECC: {:?}", spec);

    Ok(())
}
