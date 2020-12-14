use std::io::Read;

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
    pub fn new(path: &std::path::Path) -> Result<Self, std::io::Error> {
        let mut src = String::new();
        let mut src_file = std::fs::File::open(path)?;
        src_file.read_to_string(&mut src)?;

        let mut lines = Vec::new();
        let mut length = 0;
        for c in src.chars() {
            if c == '\n' {
                lines.push(length);
            }
            length += 1;
        }

        Ok(SpecFile {
            path: String::from(path.to_string_lossy()),
            src,
            lines,
            length,
        })
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
