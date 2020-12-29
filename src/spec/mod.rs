pub mod ast;
mod lex;
mod parse;

use crate::Error;
use crate::SymbolTable;

pub fn parse_spec(
    sf: &SpecFile,
) -> Result<(ast::Struct, SymbolTable), Vec<Error>> {
    let mut errors: Vec<Error> = Vec::new();
    let symtab = SymbolTable::new();
    let (stream_res, symtab, mut lerr) =
        lex::parse_token_trees(symtab, &sf.src);
    errors.append(&mut lerr);
    match stream_res {
        Ok(stream) => {
            let (spec_res, symtab, mut perr) =
                parse::parse_file_spec(symtab, stream);
            errors.append(&mut perr);
            match spec_res {
                Ok(spec) => {
                    if errors.is_empty() {
                        return Ok((spec, symtab));
                    }
                }
                Err(e) => errors.push(e),
            }
        }
        Err(e) => {
            errors.push(e);
        }
    }

    Err(errors)
}

#[derive(Debug)]
pub struct SpecFile {
    pub path: String,
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

    pub fn get_line(&self, line: u32) -> &str {
        let (start, end) = if line == 1 {
            let end = self.lines.get(0).unwrap_or(&self.length);
            (0, *end)
        } else {
            let start = *self.lines.get(line as usize - 2).unwrap_or(&0) + 1;
            let end =
                *self.lines.get(line as usize - 1).unwrap_or(&self.length);
            (start, end)
        };
        &self.src[start as usize..end as usize]
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
                self.lines[line_no - 1] + 1
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
