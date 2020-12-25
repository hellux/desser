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
        let (l1, c1) =
            self.end.map(|pos| sf.line_col(pos - 1)).unwrap_or((l0, c0));
        let lineno = format!("{}", l0);
        let ind = lineno.len();
        let line = sf.get_line(l0);
        let arrc = if l0 == l1 {
            (c1 - c0 + 1) as usize
        } else {
            line.len() + 1 - c0 as usize
        };
        let arrows = std::iter::repeat("^").take(arrc).collect::<String>();

        eprintln!("\x1b[1;31merror\x1b[0;1m: {}", self.desc);
        eprintln!(
            "{:s$}\x1b[1;34m-->\x1b[0m {}:{}:{}",
            "",
            sf.path,
            l0,
            c0,
            s = ind
        );
        eprintln!("\x1b[1;34m{:s$} |", "", s = ind);
        eprintln!("{} | \x1b[0m{}", lineno, line);
        eprintln!(
            "{:s$} \x1b[1;34m| {:c$}\x1b[1;31m{}\x1b[0m",
            "",
            "",
            arrows,
            s = ind,
            c = c0 as usize - 1
        );
        if let Some(hint) = self.hint {
            eprintln!(
                "{:s$} \x1b[1;34m= \x1b[0;1mhint\x1b[0m: {}",
                "",
                hint,
                s = ind
            );
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

    pub fn get_line(&self, line: u32) -> &str {
        let start = *self.lines.get(line as usize - 2).unwrap_or(&0) + 1;
        let end = *self.lines.get(line as usize - 1).unwrap_or(&self.length);
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
        let col = pos
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
