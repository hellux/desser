use std::collections::HashMap;
use std::io::{BufRead, Seek, SeekFrom};

use super::*;
use crate::spec::ast;
use crate::{
    AddrBase, Error, ErrorType, Span, StructSpecs, Sym, SymTraverse,
    SymbolTable,
};

pub fn parse_structure<'a, R: BufRead + Seek>(
    f: &'a mut R,
    root_spec: &'a ast::Struct,
    symtab: &SymbolTable,
) -> Result<StructuredFile, Error> {
    let mut fp = FileParser::new(f, symtab);
    let (root, _) = match fp.parse_struct(root_spec, &vec![]) {
        Ok(r) => r,
        Err(e) => return Err(Error::from(e, symtab)),
    };

    Ok(StructuredFile {
        size: fp.length,
        root,
    })
}

#[derive(Debug, PartialEq)]
enum SErrorKind {
    // errors from spec (could be checked without binary)
    StructNotInScope(Sym),
    IdentifierNotInScope(SymTraverse),
    // errors while reading binary
    InvalidValue(Val),
    EndOfFile(u64),
    AddrBeforeBase(u64),
    FailedConstraint,
}

#[derive(Debug)]
struct SError {
    span: Span,
    pos: u64,
    kind: SErrorKind,
}

type SResult<T> = Result<T, SError>;

impl Error {
    fn from(s: SError, symtab: &SymbolTable) -> Self {
        let desc = match s.kind {
            SErrorKind::StructNotInScope(sym) => {
                format!("struct '{}' not in scope", symtab.name(sym))
            }
            SErrorKind::IdentifierNotInScope(syms) => {
                format!(
                    "identifier '{}' not in scope",
                    syms.iter()
                        .map(|s| symtab.name(*s))
                        .collect::<Vec<_>>()
                        .join(".")
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
                    "end of file reached at {} while parsing field at {}",
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
            SErrorKind::FailedConstraint => {
                format!("unable to match constraint at 0x{:x}", s.pos / 8)
            }
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
enum Name {
    Struct(FieldNamespace),
    Field(Variable),
}

#[derive(Clone, Debug)]
enum Variable {
    Direct(Val),   // in memory, e.g constant or evaluated parameter
    Indirect(Ptr), // in file
}

#[derive(Clone, Debug)]
struct FieldNamespace(HashMap<Sym, Name>);

impl FieldNamespace {
    fn new() -> Self {
        FieldNamespace(HashMap::new())
    }

    fn get(&self, syms: &[Sym]) -> Option<&Variable> {
        let mut ns = &self.0;
        for i in 0..syms.len() {
            match ns.get(&syms[i]) {
                Some(Name::Struct(FieldNamespace(ss))) => ns = &ss,
                Some(Name::Field(v)) => {
                    if i == syms.len() - 1 {
                        return Some(v);
                    } else {
                        return None;
                    }
                }
                None => return None,
            }
        }

        None
    }

    fn insert_value(&mut self, sym: Sym, val: Val) {
        self.0.insert(sym, Name::Field(Variable::Direct(val)));
    }

    fn insert_field(
        &mut self,
        sym: Sym,
        kind: &StructFieldKind,
        ns: Option<FieldNamespace>,
    ) {
        if let Some(ss) = ns {
            self.0.insert(sym, Name::Struct(ss));
        } else if let StructFieldKind::Prim(ptr) = kind {
            self.0
                .insert(sym, Name::Field(Variable::Indirect(ptr.clone())));
        }
    }

    fn remove(&mut self, sym: Sym) -> Option<FieldNamespace> {
        if let Name::Struct(ns) = self.0.remove(&sym).unwrap() {
            Some(ns)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
struct CurrentStruct<'a> {
    base: u64,
    specs: &'a StructSpecs,
    fields: FieldNamespace,
}

#[derive(Clone, Debug)]
struct Scope<'a>(Vec<CurrentStruct<'a>>);

impl<'a> Scope<'a> {
    fn new() -> Self {
        Scope(Vec::new())
    }

    fn get_spec(&self, sym: Sym) -> Option<&'a ast::Struct> {
        for st in self.0.iter().rev() {
            if let Some(st) = st.specs.get(&sym) {
                return Some(st);
            }
        }

        None
    }

    fn enter_struct(
        &mut self,
        base: u64,
        specs: &'a StructSpecs,
        fields: FieldNamespace,
    ) {
        self.0.push(CurrentStruct {
            base,
            specs,
            fields,
        });
    }

    fn current(&self) -> &CurrentStruct<'a> {
        self.0.last().unwrap()
    }

    fn current_mut(&mut self) -> &mut CurrentStruct<'a> {
        self.0.last_mut().unwrap()
    }

    fn exit_struct(&mut self) -> FieldNamespace {
        self.0.pop().unwrap().fields
    }

    fn base(&self) -> u64 {
        self.current().base
    }

    fn ns(&mut self) -> &mut FieldNamespace {
        &mut self.current_mut().fields
    }
}

struct FileParser<'a, R> {
    f: &'a mut R,
    span: Span,
    pos: u64,
    length: u64,
    scope: Scope<'a>,
    sym_self: Sym,
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
            sym_self: symtab.sym_self(),
        }
    }

    fn seek(&mut self, pos: u64) -> SResult<()> {
        self.pos = pos;
        if self.pos <= self.length {
            Ok(())
        } else {
            Err(self.err(SErrorKind::EndOfFile(self.length)))
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
                    return Err(self.err(SErrorKind::AddrBeforeBase(base)));
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
                    return Err(self.err(SErrorKind::InvalidValue(al)));
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
            ast::ExprKind::Ident(id) => self.eval_id(&id),
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

    fn eval_id(&mut self, id: &SymTraverse) -> SResult<Val> {
        match self.scope.ns().get(id) {
            Some(var) => Ok(match var {
                Variable::Direct(val) => *val,
                Variable::Indirect(ptr) => ptr.eval_size(self.f),
            }),
            None => {
                Err(self.err(SErrorKind::IdentifierNotInScope(id.clone())))
            }
        }
    }

    fn parse_prim(&mut self, ptr: &Ptr) -> SResult<()> {
        self.seek(ptr.start + ptr.pty.size() as u64)?;
        Ok(())
    }

    fn parse_array(
        &mut self,
        ty: &ast::FieldType,
        size: &ast::ArraySize,
    ) -> SResult<Vec<(u64, StructFieldKind)>> {
        let mut members = Vec::new();

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

        let mut i = 0;
        loop {
            if let Some(m) = max_size {
                if i >= m as u64 {
                    break;
                }
            }

            let start = self.pos;

            // parse until constraint fails or max num is reached
            let (kind, mut ss) = match self.parse_field_kind(ty) {
                Ok(Some(field)) => field,
                Ok(None) => {
                    i += 1;
                    continue;
                }
                Err(e) => match e.kind {
                    SErrorKind::FailedConstraint
                    | SErrorKind::EndOfFile(_)
                        if i >= min_size as u64 =>
                    {
                        self.seek(start)?;
                        break;
                    }
                    _ => return Err(e),
                },
            };

            if let Some(constraint) = &ty.constraint {
                self.scope.ns().insert_field(self.sym_self, &kind, ss);
                let res = self.eval(constraint)?;
                ss = self.scope.ns().remove(self.sym_self);
                if res == 0 {
                    if i >= min_size as u64 {
                        self.seek(start)?;
                        break;
                    } else {
                        return Err(self.err(SErrorKind::FailedConstraint));
                    }
                }
            }

            // TODO add subspace to scope

            members.push((i, kind));
            i += 1
        }

        Ok(members)
    }

    fn parse_struct(
        &mut self,
        spec: &'a ast::Struct,
        params: &[Val],
    ) -> SResult<(Struct, FieldNamespace)> {
        let mut ns = FieldNamespace::new();
        for (pval, pname) in params.iter().zip(spec.parameters.iter()) {
            ns.insert_value(pname.clone(), *pval);
        }
        self.scope.enter_struct(self.pos, &spec.structs, ns);

        let start = self.pos;
        let fields = self.parse_block(&spec.block)?;
        let size = self.pos - start;

        let ns = self.scope.exit_struct();

        Ok((Struct { start, size, fields }, ns))
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
                            return Err(self.err(SErrorKind::FailedConstraint));
                        }
                    }
                }
            }
        }
        Ok(fields)
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
    ) -> SResult<Option<(StructFieldKind, Option<FieldNamespace>)>> {
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
                Some((StructFieldKind::Array(self.pos, self.parse_array(ty, count)?), None))
            }
            ast::FieldKind::Struct(id, params) => {
                let mut args = Vec::new();
                for p in params {
                    args.push(self.eval(p)?);
                }
                let struct_spec = self
                    .scope
                    .get_spec(*id)
                    .ok_or(self.err(SErrorKind::StructNotInScope(*id)))?;
                let (mut st, ss) = self.parse_struct(struct_spec, &args)?;
                match st.fields.len() {
                    0 => None,
                    1 => Some((st.fields.remove(0).1.kind, None)),
                    _ => Some((StructFieldKind::Struct(st), Some(ss)))
                }
            }
        };

        Ok(kind)
    }

    fn parse_field(&mut self, field: &ast::Field) -> SResult<Option<StructField>> {
        self.span = field.span;

        let (kind, mut ss_opt) = match self.parse_field_kind(&field.ty)? {
            Some(k) => k,
            None => return Ok(None),
        };

        if let Some(constraint) = &field.ty.constraint {
            self.scope.ns().insert_field(self.sym_self, &kind, ss_opt);
            let res = self.eval(constraint)?;
            ss_opt = self.scope.ns().remove(self.sym_self);
            if res == 0 {
                return Err(self.err(SErrorKind::FailedConstraint));
            }
        }

        if let Some(id) = field.id {
            self.scope.ns().insert_field(id, &kind, ss_opt);
        }

        Ok(Some(StructField { kind }))
    }

    fn err(&self, kind: SErrorKind) -> SError {
        SError {
            span: self.span,
            pos: self.pos,
            kind,
        }
    }
}
