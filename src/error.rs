use crate::spec::SpecFile;

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
    Structure,
}
