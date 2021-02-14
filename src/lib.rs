use std::collections::HashMap;

mod binary;
mod spec;

pub use binary::{parse_structure, view_structure};
pub use spec::{parse_spec, Span, SpecFile};

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
    pub backtrace: Vec<(u32, Option<Sym>, binary::BitPos)>,
    pub desc: String,
    pub hint: Option<String>,
    pub ty: ErrorType,
}

impl Error {
    pub fn display(&self, sf: &SpecFile, symtab: Option<&SymbolTable>) {
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
        if let Some(st) = symtab {
            for (src_pos, id_opt, bin_pos) in &self.backtrace {
                let (l, c) = sf.line_col(*src_pos);
                let id_str = id_opt.map_or("", |id| st.name(id).unwrap());
                eprintln!(
                    "{:s$}   \x1b[0m {}:{}:{} {} -- 0x{}",
                    "",
                    sf.path,
                    l,
                    c,
                    id_str,
                    bin_pos,
                    s = ind
                );
            }
        }
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
        if let Some(hint) = &self.hint {
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
#[derive(Copy, Clone, Debug)]
pub struct SpannedSym {
    sym: Sym,
    span: Span,
}

#[derive(Copy, Clone, Debug)]
enum BuiltInIdent {
    IdSelf,
    Super,
}

#[derive(Copy, Clone, Debug)]
enum BuiltInProp {
    Order,
    OrderBit,

    Final,
    Constraint,
    Zero,
    NonZero,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,

    Align,
    AlignBit,

    Addr,
    Skip,
    Offset,
    AddrBit,
    SkipBit,
    OffsetBit,
}

#[derive(Copy, Clone, Debug)]
enum BuiltInAttr {
    Start,
    Size,
    End,
    Offset,
    Length,
}

const IDENTIFIERS: [(BuiltInIdent, &str); 2] = [
    (BuiltInIdent::IdSelf, "#self"),
    (BuiltInIdent::Super, "super"),
];
const PROPERTIES: [(BuiltInProp, &str); 20] = [
    (BuiltInProp::Order, "order"),
    (BuiltInProp::OrderBit, "border"),
    (BuiltInProp::Final, "final"),
    (BuiltInProp::Constraint, "constraint"),
    (BuiltInProp::Zero, "zero"),
    (BuiltInProp::NonZero, "nonzero"),
    (BuiltInProp::Eq, "eq"),
    (BuiltInProp::Neq, "neq"),
    (BuiltInProp::Lt, "lt"),
    (BuiltInProp::Gt, "gt"),
    (BuiltInProp::Leq, "leq"),
    (BuiltInProp::Geq, "geq"),
    (BuiltInProp::Align, "align"),
    (BuiltInProp::AlignBit, "balign"),
    (BuiltInProp::Addr, "addr"),
    (BuiltInProp::Skip, "skip"),
    (BuiltInProp::Offset, "offset"),
    (BuiltInProp::AddrBit, "baddr"),
    (BuiltInProp::SkipBit, "bskip"),
    (BuiltInProp::OffsetBit, "boffset"),
];
const ATTRIBUTES: [(BuiltInAttr, &str); 5] = [
    (BuiltInAttr::Start, "start"),
    (BuiltInAttr::Size, "size"),
    (BuiltInAttr::End, "end"),
    (BuiltInAttr::Offset, "offset"),
    (BuiltInAttr::Length, "length"),
];

#[derive(Clone, Debug)]
pub struct SymbolTable {
    map: HashMap<String, Sym>,
    arr: Vec<String>,
    attributes: HashMap<Sym, BuiltInAttr>,
    properties: HashMap<Sym, BuiltInProp>,
}

impl SymbolTable {
    fn new() -> Self {
        let mut tbl = SymbolTable {
            map: HashMap::new(),
            arr: Vec::new(),
            attributes: HashMap::new(),
            properties: HashMap::new(),
        };

        for (_, name) in IDENTIFIERS.iter() {
            tbl.insert(name);
        }

        for (prop, name) in PROPERTIES.iter() {
            let sym = tbl.insert(name);
            tbl.properties.insert(sym, *prop);
        }

        for (attr, name) in ATTRIBUTES.iter() {
            let sym = tbl.insert(name);
            tbl.attributes.insert(sym, *attr);
        }

        tbl
    }

    pub fn name(&self, sym: Sym) -> Option<&str> {
        self.arr.get(sym as usize).map(String::as_str)
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

    fn ident_sym(&self, b: BuiltInIdent) -> Sym {
        *self
            .map
            .get(IDENTIFIERS.get(b as usize).unwrap().1)
            .unwrap()
    }

    fn property(&self, sym: Sym) -> Option<BuiltInProp> {
        self.properties.get(&sym).copied()
    }

    fn attribute(&self, sym: Sym) -> Option<BuiltInAttr> {
        self.attributes.get(&sym).copied()
    }
}
