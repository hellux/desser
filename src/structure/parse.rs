use std::collections::HashMap;
use std::io::{BufRead, Seek, SeekFrom};

use super::*;
use crate::spec::ast;
use crate::{AddrBase, Error, ErrorType, Span, StructSpecs, Sym, SymbolTable};

pub fn parse_structure<'a, R: BufRead + Seek>(
    f: &'a mut R,
    root_spec: &'a ast::Struct,
    symtab: &SymbolTable,
) -> Result<StructuredFile, Error> {
    let mut fp = FileParser::new(f, symtab);
    let (root, _) = match fp.parse_struct(root_spec, &vec![]) {
        Ok(r) => r,
        Err(kind) => {
            let e = SError {
                span: fp.span,
                pos: fp.pos,
                kind,
            };
            return Err(Error::from(e, symtab));
        }
    };

    Ok(StructuredFile {
        size: fp.length,
        root,
    })
}

#[derive(Debug)]
enum SErrorKind {
    // errors from spec (could potentially be checked without binary)
    StructNotInScope(Sym),
    IdentifierNotInScope(Sym),
    NonStructMemberAccess,
    NonArrayIndexAccess,

    // errors while reading binary
    SelfNotValid,
    NotAVariable,
    NotAStructOrArray,
    IndexNotFound(u64),
    InvalidValue(Val),
    EndOfFile(u64),
    AddrBeforeBase(u64),
    FailedConstraint(i64),
}

#[derive(Debug)]
struct SError {
    span: Span,
    pos: u64,
    kind: SErrorKind,
}

type SResult<T> = Result<T, SErrorKind>;

impl Error {
    fn from(s: SError, symtab: &SymbolTable) -> Self {
        let desc = match s.kind {
            SErrorKind::StructNotInScope(sym) => {
                format!("struct '{}' not in scope", symtab.name(sym))
            }
            SErrorKind::IdentifierNotInScope(sym) => {
                format!(
                    "identifier '{}' not in scope",
                    String::from(symtab.name(sym)),
                )
            }
            SErrorKind::InvalidValue(val) => {
                format!(
                    "value '{}' is not valid here at 0x{:x}",
                    val,
                    s.pos / 8
                )
            }
            SErrorKind::EndOfFile(size) => {
                format!(
                    "end of file reached at {:x} while parsing field at {:x}",
                    size / 8,
                    s.pos / 8
                )
            }
            SErrorKind::AddrBeforeBase(base) => {
                format!(
                    "jump to {:x} which is before current struct base at {:x}",
                    s.pos / 8,
                    base / 8
                )
            }
            SErrorKind::FailedConstraint(res) => {
                format!(
                    "unable to match constraint at 0x{:x} -- {}",
                    s.pos / 8,
                    res
                )
            }
            k => format!("{:?}", k),
        };
        let hint = None;

        Error {
            span: s.span,
            desc,
            hint,
            ty: ErrorType::Structure,
        }
    }
}

#[derive(Clone, Debug)]
enum Variable {
    Direct(Val),   // in memory, e.g constant or evaluated parameter
    Indirect(Ptr), // in file
}

#[derive(Clone, Debug)]
enum Name {
    Subspace(Namespace),
    Field(Variable),
}

impl Name {
    fn new(kind: &StructFieldKind, ns: Option<Namespace>) -> Option<Name> {
        if let Some(ss) = ns {
            Some(Name::Subspace(ss))
        } else if let StructFieldKind::Prim(ptr) = kind {
            Some(Name::Field(Variable::Indirect(ptr.clone())))
        } else if let None = ns {
            None
        } else {
            unimplemented!()
        }
    }

    fn get(&self, syms: &[SymAccess]) -> SResult<&Variable> {
        match self {
            Name::Subspace(ss) => ss.get(syms),
            Name::Field(v) => {
                if syms.len() == 0 {
                    Ok(&v)
                } else {
                    Err(SErrorKind::NotAStructOrArray)
                }
            }
        }
    }

    fn ss(self) -> Option<Namespace> {
        match self {
            Name::Subspace(ns) => Some(ns),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
enum Namespace {
    Struct(HashMap<Sym, Name>),
    Array(HashMap<u64, Name>),
}

impl Namespace {
    fn get(&self, syms: &[SymAccess]) -> SResult<&Variable> {
        let head = &syms.get(0).ok_or(SErrorKind::NotAVariable)?;
        let name = match head {
            SymAccess::Sym(sym) => match self {
                Namespace::Struct(space) => space
                    .get(&sym)
                    .ok_or(SErrorKind::IdentifierNotInScope(*sym))?,
                _ => return Err(SErrorKind::NonStructMemberAccess),
            },
            SymAccess::Index(idx) => match self {
                Namespace::Array(space) => {
                    space.get(&idx).ok_or(SErrorKind::IndexNotFound(*idx))?
                }
                _ => return Err(SErrorKind::NonArrayIndexAccess),
            },
        };

        name.get(&syms[1..])
    }

    fn insert_field(
        &mut self,
        sym: Sym,
        kind: &StructFieldKind,
        ns: Option<Namespace>,
    ) {
        if let Namespace::Struct(space) = self {
            if let Some(name) = Name::new(kind, ns) {
                space.insert(sym, name);
            }
        } else {
            panic!();
        }
    }

    fn insert_element(
        &mut self,
        idx: u64,
        kind: &StructFieldKind,
        ns: Option<Namespace>,
    ) {
        if let Namespace::Array(space) = self {
            if let Some(name) = Name::new(kind, ns) {
                space.insert(idx, name);
            }
        } else {
            panic!();
        }
    }
}

#[derive(Clone, Debug)]
struct Space<'a> {
    base: u64,
    fields: Namespace,
    specs: Option<&'a StructSpecs>,
}

#[derive(Clone, Debug)]
struct Scope<'a>(Vec<Space<'a>>);

impl<'a> Scope<'a> {
    fn new() -> Self {
        Scope(Vec::new())
    }

    fn get_spec(&self, sym: Sym) -> Option<&'a ast::Struct> {
        for st in self.0.iter().rev() {
            if let Some(st) = st.specs.and_then(|s| s.get(&sym)) {
                return Some(st);
            }
        }

        None
    }

    fn enter_struct(
        &mut self,
        base: u64,
        st: &'a ast::Struct,
        actual_params: &[Val],
    ) {
        let mut names = HashMap::new();
        for (pname, pval) in st.parameters.iter().zip(actual_params.iter()) {
            names.insert(pname.clone(), Name::Field(Variable::Direct(*pval)));
        }
        let ns = Namespace::Struct(names);
        self.0.push(Space {
            base,
            fields: ns,
            specs: Some(&st.structs),
        });
    }

    fn exit_struct(&mut self) -> Namespace {
        self.0.pop().unwrap().fields
    }

    fn base(&self) -> u64 {
        self.0.last().unwrap().base
    }

    fn ns(&mut self) -> &mut Namespace {
        &mut self.0.last_mut().unwrap().fields
    }
}

struct FileParser<'a, R> {
    f: &'a mut R,
    span: Span,
    pos: u64,
    length: u64,
    scope: Scope<'a>,
    self_sym: Sym,
    self_name: Option<Name>,
}

impl<'a, R: BufRead + Seek> FileParser<'a, R> {
    fn new(f: &'a mut R, symtab: &SymbolTable) -> Self {
        let length = f.seek(SeekFrom::End(0)).unwrap() * 8;
        FileParser {
            f,
            span: Span(0, 0),
            pos: 0,
            length,
            scope: Scope::new(),
            self_sym: symtab.sym_self(),
            self_name: None,
        }
    }

    fn seek(&mut self, pos: u64) -> SResult<()> {
        self.pos = pos;
        if self.pos <= self.length {
            Ok(())
        } else {
            Err(SErrorKind::EndOfFile(self.length))
        }
    }

    fn seek_loc(&mut self, loc: &ast::Location) -> SResult<()> {
        let base = self.scope.base();
        match &loc.expr {
            Some(expr) => {
                let base = match loc.base {
                    AddrBase::Absolute => 0,
                    AddrBase::Relative => self.pos,
                    AddrBase::Local => base,
                };

                let offset =
                    self.eval(expr)? as u64 * if loc.bitwise { 1 } else { 8 };
                self.seek(base + offset)?;
                if self.pos < base {
                    return Err(SErrorKind::AddrBeforeBase(base));
                }
            }
            None => {}
        };

        Ok(())
    }

    fn align(&mut self, alignment: &ast::Alignment) -> SResult<()> {
        match &alignment.expr {
            Some(expr) => {
                let al = self.eval(expr)?;
                if al > 0 {
                    let al = al as u64 * if alignment.bitwise { 1 } else { 8 };
                    if self.pos % al > 0 {
                        self.pos += al - self.pos % al
                    }
                } else {
                    return Err(SErrorKind::InvalidValue(al));
                }
            }
            None => {}
        };

        Ok(())
    }

    fn eval(&mut self, expr: &ast::Expr) -> SResult<Val> {
        self.span = expr.span;
        let val = match &expr.kind {
            ast::ExprKind::Int(val) => Ok(*val),
            ast::ExprKind::Ident(id) => self.eval_id(id),
            ast::ExprKind::Binary(binop) => {
                let left = self.eval(&binop.lhs)?;
                let right = self.eval(&binop.rhs)?;
                Ok(match binop.kind {
                    ast::BinOpKind::Add => left + right,
                    ast::BinOpKind::Sub => left - right,
                    ast::BinOpKind::Mul => left * right,
                    ast::BinOpKind::Div => left / right,
                    ast::BinOpKind::Rem => left % right,
                    ast::BinOpKind::BitAnd => left & right,
                    ast::BinOpKind::BitXor => left ^ right,
                    ast::BinOpKind::BitOr => left | right,
                    ast::BinOpKind::Eq => (left == right) as Val,
                    ast::BinOpKind::Neq => (left != right) as Val,
                    ast::BinOpKind::Lt => (left < right) as Val,
                    ast::BinOpKind::Gt => (left > right) as Val,
                    ast::BinOpKind::Leq => (left <= right) as Val,
                    ast::BinOpKind::Geq => (left >= right) as Val,
                    ast::BinOpKind::Shl => left << right,
                    ast::BinOpKind::Shr => left >> right,
                })
            }
            ast::ExprKind::Unary(unop) => {
                let expr = self.eval(&unop.expr)?;
                Ok(match unop.kind {
                    ast::UnOpKind::Neg => -expr,
                    ast::UnOpKind::Not => {
                        if expr == 0 {
                            1
                        } else {
                            0
                        }
                    }
                })
            }
        };
        self.span = expr.span;
        val
    }

    fn eval_id(&mut self, id: &[ast::SymAccess]) -> SResult<Val> {
        let mut id_conv = Vec::new();
        for sa in id {
            id_conv.push(self.convert_sa(sa)?);
        }

        let var = match id_conv[0] {
            SymAccess::Sym(sym) if sym == self.self_sym => {
                if let Some(name) = &self.self_name {
                    match name {
                        Name::Subspace(ns) => ns.get(&id_conv[1..])?,
                        Name::Field(var) => var,
                    }
                } else {
                    return Err(SErrorKind::SelfNotValid);
                }
            }
            _ => self.scope.ns().get(id_conv.as_slice())?,
        };

        Ok(match var {
            Variable::Direct(val) => *val,
            Variable::Indirect(ptr) => ptr.eval_size(self.f),
        })
    }

    fn parse_prim(&mut self, ptr: &Ptr) -> SResult<()> {
        self.seek(ptr.start + ptr.pty.size() as u64)?;
        Ok(())
    }

    fn parse_array(
        &mut self,
        ty: &ast::FieldType,
        size: &ast::ArraySize,
    ) -> SResult<(Array, Namespace)> {
        let mut elements = Vec::new();

        let (min_size, max_size) = match size {
            ast::ArraySize::Exactly(n) => {
                let n = self.eval(n)?;
                (n, Some(n))
            }
            ast::ArraySize::Within(a, b) => {
                (self.eval(a)?, Some(self.eval(b)?))
            }
            ast::ArraySize::AtLeast(n) => (self.eval(n)?, None),
        };

        let start = self.pos;

        let mut ns = Namespace::Array(HashMap::new());
        let mut i = 0;
        loop {
            if let Some(m) = max_size {
                if i >= m as u64 {
                    break;
                }
            }

            let elem_start = self.pos;

            // parse until constraint fails or max num is reached
            let (kind, mut ss_opt) = match self.parse_field_kind(ty) {
                Ok(Some(field)) => field,
                Ok(None) => {
                    i += 1;
                    continue;
                }
                Err(k) => match k {
                    SErrorKind::FailedConstraint(_)
                    | SErrorKind::EndOfFile(_)
                        if i >= min_size as u64 =>
                    {
                        self.seek(elem_start)?;
                        break;
                    }
                    _ => return Err(k),
                },
            };

            if let Some(constraint) = &ty.constraint {
                self.self_name = Name::new(&kind, ss_opt);
                let res = self.eval(constraint)?;
                ss_opt = self.self_name.take().unwrap().ss();
                if res == 0 {
                    if i >= min_size as u64 {
                        self.seek(elem_start)?;
                        break;
                    } else {
                        return Err(SErrorKind::FailedConstraint(res));
                    }
                }
            }

            ns.insert_element(i, &kind, ss_opt);

            elements.push((i, kind));
            i += 1
        }
        let size = self.pos - start;

        Ok((
            Array {
                start,
                size,
                elements,
            },
            ns,
        ))
    }

    fn parse_struct(
        &mut self,
        spec: &'a ast::Struct,
        params: &[Val],
    ) -> SResult<(Struct, Namespace)> {
        self.scope.enter_struct(self.pos, &spec, params);

        let start = self.pos;
        let fields = self.parse_block(&spec.block)?;
        let size = self.pos - start;

        let ns = self.scope.exit_struct();

        Ok((
            Struct {
                start,
                size,
                fields,
            },
            ns,
        ))
    }

    fn parse_block(
        &mut self,
        block: &ast::Block,
    ) -> SResult<Vec<(Option<Sym>, StructField)>> {
        let mut fields = Vec::new();
        for s in block {
            match s {
                ast::Stmt::Field(f) => {
                    if let Some(field) = self.parse_field(&f)? {
                        if !f.hidden {
                            fields.push((f.id.clone(), field));
                        }
                    }
                }
                ast::Stmt::If(if_stmt) => {
                    let body = if self.eval(&if_stmt.cond)? != 0 {
                        &if_stmt.if_body
                    } else {
                        let mut i = 0;
                        loop {
                            if let Some((c, b)) = if_stmt.elseifs.get(i) {
                                if self.eval(c)? != 0 {
                                    break b;
                                } else {
                                    i += 1;
                                }
                            } else {
                                break &if_stmt.else_body;
                            }
                        }
                    };

                    fields.append(&mut self.parse_block(body)?);
                }
                ast::Stmt::Constrain(exprs) => {
                    for expr in exprs {
                        let res = self.eval(expr)?;
                        if res == 0 {
                            return Err(SErrorKind::FailedConstraint(res));
                        }
                    }
                }
                ast::Stmt::Debug(exprs) => {
                    for expr in exprs {
                        let res = self.eval(expr)?;
                        eprintln!("debug: {}", res);
                    }
                }
            }
        }
        Ok(fields)
    }

    fn convert_sa(&mut self, asa: &ast::SymAccess) -> SResult<SymAccess> {
        Ok(match asa {
            ast::SymAccess::Sym(sym) => SymAccess::Sym(*sym),
            ast::SymAccess::Index(expr) => {
                SymAccess::Index(self.eval(expr)? as u64)
            }
        })
    }

    fn convert_prim(&mut self, apty: &ast::PrimType) -> SResult<PrimType> {
        Ok(match apty {
            ast::PrimType::Signed(len) => {
                PrimType::Signed(self.eval(&len)? as u8)
            }
            ast::PrimType::Unsigned(len) => {
                PrimType::Unsigned(self.eval(&len)? as u8)
            }
            ast::PrimType::Float(exponent, mantissa) => PrimType::Float(
                self.eval(&exponent)? as u8,
                self.eval(&mantissa)? as u8,
            ),
            ast::PrimType::BitVec(len) => {
                PrimType::BitVec(self.eval(&len)? as u8)
            }
            ast::PrimType::Char => PrimType::Char,
            ast::PrimType::U8 => PrimType::U8,
            ast::PrimType::S8 => PrimType::S8,
            ast::PrimType::U16 => PrimType::U16,
            ast::PrimType::S16 => PrimType::S16,
            ast::PrimType::U32 => PrimType::U32,
            ast::PrimType::S32 => PrimType::S32,
            ast::PrimType::U64 => PrimType::U64,
            ast::PrimType::S64 => PrimType::S64,
            ast::PrimType::U128 => PrimType::U128,
            ast::PrimType::S128 => PrimType::S128,
            ast::PrimType::F32 => PrimType::F32,
            ast::PrimType::F64 => PrimType::F64,
        })
    }

    fn parse_field_kind(
        &mut self,
        ty: &ast::FieldType,
    ) -> SResult<Option<(StructFieldKind, Option<Namespace>)>> {
        self.seek_loc(&ty.loc)?;
        self.align(&ty.alignment)?;

        let kind = match &ty.kind {
            ast::FieldKind::Prim(apty) => {
                let spty = self.convert_prim(&apty)?;
                let ptr = Ptr {
                    start: self.pos,
                    pty: spty,
                    byte_order: ty.byte_order,
                };
                self.parse_prim(&ptr)?;
                Some((StructFieldKind::Prim(ptr), None))
            }
            ast::FieldKind::Array(ty, count) => {
                let (arr, ss) = self.parse_array(ty, count)?;
                Some((StructFieldKind::Array(arr), Some(ss)))
            }
            ast::FieldKind::Struct(id, params) => {
                let mut args = Vec::new();
                for p in params {
                    args.push(self.eval(p)?);
                }
                let struct_spec = self
                    .scope
                    .get_spec(*id)
                    .ok_or(SErrorKind::StructNotInScope(*id))?;
                let (mut st, ss) = self.parse_struct(struct_spec, &args)?;
                match st.fields.len() {
                    0 => None,
                    1 => Some((st.fields.remove(0).1.kind, None)),
                    _ => Some((StructFieldKind::Struct(st), Some(ss))),
                }
            }
        };

        Ok(kind)
    }

    fn parse_field(
        &mut self,
        field: &ast::Field,
    ) -> SResult<Option<StructField>> {
        self.span = field.span;

        let (kind, mut ss_opt) = match self.parse_field_kind(&field.ty)? {
            Some(k) => k,
            None => return Ok(None),
        };

        if let Some(constraint) = &field.ty.constraint {
            self.self_name = Name::new(&kind, ss_opt);
            let res = self.eval(constraint)?;
            ss_opt = self.self_name.take().unwrap().ss();
            if res == 0 {
                return Err(SErrorKind::FailedConstraint(res));
            }
        }

        if let Some(id) = field.id {
            self.scope.ns().insert_field(id, &kind, ss_opt);
        }

        Ok(Some(StructField { kind }))
    }
}
