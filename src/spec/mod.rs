pub mod ast;
mod lex;
mod parse;

use crate::sym;

pub fn parse_spec(
    sf: &SpecFile,
) -> Result<(ast::Struct, sym::SymbolTable), ()> {
    let mut errors: Vec<Error> = Vec::new();
    let symtab = sym::SymbolTable::new();
    let (stream_res, symtab, mut lerr) =
        lex::parse_token_trees(symtab, &sf.src);
    if lerr.is_empty() {
        match stream_res {
            Ok(stream) => {
                let (spec_res, symtab, mut perr) =
                    parse::parse_file_spec(symtab, stream);
                if perr.is_empty() {
                    match spec_res {
                        Ok(spec) => return Ok((spec, symtab)),
                        Err(e) => errors.push(e),
                    }
                } else {
                    errors.append(&mut perr);
                }
            }
            Err(e) => {
                errors.push(e);
            }
        }
    } else {
        errors.append(&mut lerr);
    }

    for e in errors {
        e.display(sf);
    }

    Err(())
}

#[derive(Debug)]
pub struct Error {
    pub start: u32,
    pub end: Option<u32>,
    pub desc: String,
    pub hint: Option<&'static str>,
    pub ty: ErrorType,
}

impl Error {
    pub fn display(&self, sf: &SpecFile) {
        let (l0, c0) = sf.line_col(self.start);
        eprint!("error: {}\n  --> {}:{}:{}\n", self.desc, sf.path, l0, c0);
        if let Some(hint) = self.hint {
            eprintln!("  hint: {}", hint);
        }
    }
}

#[derive(Debug)]
pub enum ErrorType {
    Lexical,
    Parsing,
}

#[derive(Debug)]
pub struct SpecFile {
    path: String,
    pub src: String,
    lines: Vec<u32>,
    length: u32,
}

impl SpecFile {
    pub fn new(path: &str, src: String) -> Self {
        let mut lines = Vec::new();
        let mut length = 0;
        for c in src.chars() {
            if c == '\n' {
                lines.push(length);
            }
            length += 1;
        }

        SpecFile {
            path: path.to_string(),
            src,
            lines,
            length,
        }
    }

    pub fn line_col(&self, pos: u32) -> (u32, u32) {
        let mut line_no = 0;
        for l in &self.lines {
            if &pos < l {
                break;
            }
            line_no += 1;
        }

        let line = (line_no + 1) as u32;
        let col = pos + 1
            - if line_no > 0 {
                self.lines[line_no - 1]
            } else {
                0
            };

        (line, col)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span(lo as u32, hi as u32)
    }
}
