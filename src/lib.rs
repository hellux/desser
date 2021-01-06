use std::collections::HashMap;

mod spec;
mod structure;

pub use spec::{parse_spec, Span, SpecFile};
pub use structure::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Order {
    LittleEndian,
    BigEndian,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AddrBase {
    Absolute, // 0 at file start
    Relative, // 0 at current position
    Local,    // 0 at current struct base
}

#[derive(Copy, Clone, Debug)]
pub enum PrimType<T> {
    //    Int(T),
    //    Uint(T),
    //    Float(T, T), // exponent, mantissa
    BitVec(T),
    Char,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub desc: String,
    pub hint: Option<&'static str>,
    pub ty: ErrorType,
}

impl Error {
    pub fn display(&self, sf: &SpecFile) {
        let (l0, c0) = sf.line_col(self.span.0);
        let (l1, c1) = sf.line_col(self.span.1 - 1);
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
    Structure,
}

pub type Sym = u16;
#[derive(Clone, Debug)]
pub enum SymAccess<T> {
    Sym(Sym),
    Index(T),
}
type StructSpecs = HashMap<Sym, spec::ast::Struct>;

#[derive(Clone, Debug)]
pub struct SymbolTable {
    map: HashMap<String, Sym>,
    arr: Vec<String>,
}

pub const SELF_NAME: &str = "self";

impl SymbolTable {
    fn new() -> Self {
        let mut tbl = SymbolTable {
            map: HashMap::new(),
            arr: Vec::new(),
        };
        tbl.insert(SELF_NAME);
        tbl
    }

    pub fn name(&self, sym: Sym) -> Option<&str> {
        self.arr.get(sym as usize).map(|s| s.as_str())
    }

    fn insert(&mut self, name: &str) -> Sym {
        if let Some(sym) = self.map.get(name) {
            *sym
        } else {
            let sym = self.arr.len() as Sym;
            self.arr.push(String::from(name));
            self.map.insert(String::from(name), sym);
            sym
        }
    }

    fn sym_self(&self) -> Sym {
        *self.map.get(SELF_NAME).unwrap()
    }
}
