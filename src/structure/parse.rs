use std::collections::HashMap;
use std::io::{BufRead, Seek, SeekFrom};

use super::*;
use crate::spec::ast;
use crate::{AddrBase, Error, ErrorType, Span, StructSpecs, Sym, SymTraverse, SymbolTable};

pub fn parse_structure<'a, R: BufRead + Seek>(
    f: &'a mut R,
    root_spec: &'a ast::Struct,
    symtab: &SymbolTable,
) -> Result<StructuredFile, Error> {
    let mut fp = FileParser::new(f);
    let (root, _) = match fp.parse_struct(root_spec, &vec![]) {
        Ok(r) => r,
        Err(e) => return Err(Error::from(e, symtab)),
    };

    Ok(StructuredFile {
        size: fp.length,
        root,
    })
}

#[derive(Debug)]
enum SErrorKind {
    EndOfFile(u64, u64),
    StructNotInScope(Sym),
    IdentifierNotInScope(SymTraverse),
    InvalidValue(Val),
    AddrBeforeBase(u64, u64),
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
            SErrorKind::EndOfFile(addr, size) => {
                format!(
                    "end of file reached at {} while parsing field at {}",
                    size, addr
                )
            }
            SErrorKind::AddrBeforeBase(addr, base) => {
                format!(
                    "jump to {} which is before current struct base at {}",
                    addr, base
                )
            }
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
                format!("value '{}' is not valid here", val)
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

    fn insert_pointer(&mut self, sym: Sym, ptr: Ptr) {
        self.0.insert(sym, Name::Field(Variable::Indirect(ptr)));
    }

    fn insert_struct(&mut self, sym: Sym, ns: FieldNamespace) {
        self.0.insert(sym, Name::Struct(ns));
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

    fn exit_struct(&mut self) -> FieldNamespace {
        self.0.pop().unwrap().fields
    }

    fn base(&self) -> u64 {
        self.0.last().unwrap().base
    }

    fn ns(&mut self) -> &mut FieldNamespace {
        &mut self.0.last_mut().unwrap().fields
    }
}

struct FileParser<'a, R> {
    f: &'a mut R,
    span: Span,
    pos: u64,
    length: u64,
    scope: Scope<'a>,
}

impl<'a, R: BufRead + Seek> FileParser<'a, R> {
    fn new(f: &'a mut R) -> Self {
        let length = f.seek(SeekFrom::End(0)).unwrap() * 8;
        FileParser {
            f,
            span: Span(0, 0),
            pos: 0,
            length,
            scope: Scope::new(),
        }
    }

    fn seek(&mut self, from: SeekFrom) -> SResult<u64> {
        let new_pos = match from {
            SeekFrom::Start(addr) => addr,
            SeekFrom::Current(rel_addr) => (self.pos as i64 + rel_addr) as u64,
            SeekFrom::End(rel_addr) => (self.length as i64 + rel_addr) as u64,
        };

        if new_pos <= self.length {
            self.pos = new_pos;
            Ok(self.pos)
        } else {
            Err(self.err(SErrorKind::EndOfFile(new_pos / 8, self.length / 8)))
        }
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
                    ast::BinOpKind::Shl => left << right,
                    ast::BinOpKind::Shr => left >> right,
                })
            }
            ast::ExprKind::Unary(unop) => {
                let expr = self.eval(&unop.expr)?;
                Ok(match unop.kind {
                    ast::UnOpKind::Neg => -expr,
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

    fn goto(&mut self, loc: &ast::Location) -> SResult<()> {
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
                let new_pos = base + offset;
                if new_pos >= base {
                    self.pos = new_pos;
                } else {
                    return Err(
                        self.err(SErrorKind::AddrBeforeBase(new_pos, base))
                    );
                }
            }
            None => {}
        };

        Ok(())
    }

    fn parse_prim(&mut self, ptr: &Ptr) -> SResult<()> {
        self.seek(SeekFrom::Start(ptr.start + ptr.pty.size() as u64))?;
        Ok(())
    }

    fn parse_array(
        &mut self,
        kind: &ast::FieldType,
        size: &ast::ArraySize,
    ) -> SResult<Vec<StructFieldKind>> {
        let mut members = Vec::new();

        let max_size = match size {
            ast::ArraySize::Exactly(n) => Some(self.eval(n)?),
            ast::ArraySize::Within(_, n) => Some(self.eval(n)?),
            ast::ArraySize::AtLeast(_) => None,
        };

        let mut n = 0;
        loop {
            if let Some(m) = max_size {
                if n >= m {
                    break;
                }
            }

            self.goto(&kind.loc)?;
            self.align(&kind.alignment)?;
            let (member, _) = self.parse_field_kind(kind)?;
            members.push(member);

            // TODO stop on invalid member
            n += 1;
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

        Ok((Struct { size, fields }, ns))
    }

    fn parse_block(
        &mut self,
        block: &ast::Block,
    ) -> SResult<Vec<(Option<Sym>, StructField)>> {
        let mut fields = Vec::new();
        for s in block {
            match s {
                ast::Stmt::Field(f) => {
                    let (field, ss_opt) = self.parse_field(&f)?;
                    if let Some(id) = f.id {
                        if let Some(ss) = ss_opt {
                            self.scope.ns().insert_struct(id, ss);
                        } else if let StructFieldKind::Prim(ptr) = &field.kind
                        {
                            self.scope.ns().insert_pointer(id, ptr.clone());
                        }
                    }
                    fields.push((f.id.clone(), field));
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
    ) -> SResult<(StructFieldKind, Option<FieldNamespace>)> {
        Ok(match &ty.kind {
            ast::FieldKind::Prim(apty) => {
                let spty = self.convert_prim(&apty)?;
                let ptr = Ptr {
                    start: self.pos,
                    pty: spty,
                    byte_order: ty.byte_order,
                };
                self.parse_prim(&ptr)?;
                (StructFieldKind::Prim(ptr), None)
            }
            ast::FieldKind::Array(ty, count) => {
                (StructFieldKind::Array(self.parse_array(ty, count)?), None)
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
                let (st, ss) = self.parse_struct(struct_spec, &args)?;
                (StructFieldKind::Struct(st), Some(ss))
            }
        })
    }

    fn parse_field(
        &mut self,
        field: &ast::Field,
    ) -> SResult<(StructField, Option<FieldNamespace>)> {
        self.span = field.span;

        self.goto(&field.ty.loc)?;
        self.align(&field.ty.alignment)?;

        let (kind, ss) = self.parse_field_kind(&field.ty)?;

        Ok((StructField { kind }, ss))
    }

    fn err(&self, kind: SErrorKind) -> SError {
        SError {
            span: self.span,
            pos: self.pos,
            kind,
        }
    }
}
