use std::collections::HashMap;
use std::io::{BufRead, Seek, SeekFrom};

use crate::error::{Error, ErrorType};
use crate::spec::ast;
use crate::spec::Span;
use crate::structure::*;
use crate::sym;

#[derive(Debug)]
pub enum SErrorKind {
    EndOfFile,
    StructNotInScope(sym::Sym),
    IdentifierNotInScope(Vec<sym::Sym>),
}

#[derive(Debug)]
pub struct SError {
    span: Span,
    pos: u64,
    kind: SErrorKind,
}

pub type SResult<T> = Result<T, SError>;

impl Error {
    fn from(s: SError, symtab: &sym::SymbolTable) -> Self {
        let start = s.span.0;
        let end = Some(s.span.1);
        let desc = match s.kind {
            SErrorKind::EndOfFile => {
                format!("end of file reached while parsing field")
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
        };
        let hint = None;

        Error {
            start,
            end,
            desc,
            hint,
            ty: ErrorType::Structure,
        }
    }
}

pub struct FileParser<R> {
    f: R,
    span: Span,
    pos: u64,
    length: u64,
}

impl<R: BufRead + Seek> FileParser<R> {
    pub fn new(mut f: R) -> Self {
        let length = f.seek(SeekFrom::End(0)).unwrap() * 8;
        FileParser {
            f,
            span: Span(0, 0),
            pos: 0,
            length,
        }
    }

    pub fn parse(
        &mut self,
        root_spec: &ast::Struct,
        symtab: &sym::SymbolTable,
    ) -> Result<StructuredFile, Error> {
        let (root, _) = match self.parse_struct(root_spec, &vec![]) {
            Ok(r) => r,
            Err(e) => return Err(Error::from(e, symtab)),
        };

        Ok(StructuredFile {
            size: self.length,
            root,
        })
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
            Err(self.err(SErrorKind::EndOfFile))
        }
    }

    pub fn eval(
        &mut self,
        expr: &ast::Expr,
        ns: &sym::Namespace,
    ) -> SResult<Val> {
        match expr {
            ast::Expr::Int(val) => Ok(*val),
            ast::Expr::Ident(id) => self.eval_id(id, ns),
            ast::Expr::Binary(binop) => {
                let left = self.eval(&binop.lhs, ns)?;
                let right = self.eval(&binop.rhs, ns)?;
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
            ast::Expr::Unary(unop) => {
                let expr = self.eval(&unop.expr, ns)?;
                Ok(match unop.kind {
                    ast::UnOpKind::Neg => -expr,
                })
            }
        }
    }

    fn eval_id(
        &mut self,
        id: &Vec<sym::Sym>,
        ns: &sym::Namespace,
    ) -> SResult<Val> {
        match ns.get(id) {
            Some(var) => Ok(match var {
                sym::Variable::Direct(val) => *val,
                sym::Variable::Indirect(ptr) => ptr.eval_size(&mut self.f),
            }),
            None => {
                Err(self.err(SErrorKind::IdentifierNotInScope(id.clone())))
            }
        }
    }

    fn parse_prim(&mut self, ptr: &Ptr) -> SResult<()> {
        self.seek(SeekFrom::Start(ptr.start + ptr.pty.size() as u64))?;
        Ok(())
    }

    fn parse_array(
        &mut self,
        specs: &HashMap<sym::Sym, ast::Struct>,
        ns: &sym::Namespace,
        kind: &ast::FieldType,
        size: &ast::ArraySize,
    ) -> SResult<Vec<StructFieldKind>> {
        let mut members = Vec::new();

        let max_size = match size {
            ast::ArraySize::Exactly(n) => Some(self.eval(n, ns)?),
            ast::ArraySize::Within(_, n) => Some(self.eval(n, ns)?),
            ast::ArraySize::AtLeast(_) => None,
        };

        let mut n = 0;
        loop {
            if let Some(m) = max_size {
                if n >= m {
                    break;
                }
            }

            let (member, _) = self.parse_field_kind(specs, ns, kind)?;
            members.push(member);

            // TODO stop on invalid member
            n += 1;
        }

        Ok(members)
    }

    fn parse_struct(
        &mut self,
        spec: &ast::Struct,
        params: &Vec<Val>,
    ) -> SResult<(Struct, sym::Namespace)> {
        let mut ns = sym::Namespace::new();

        for (pval, pname) in params.iter().zip(spec.parameters.iter()) {
            ns.insert_value(pname.clone(), *pval);
        }

        let start = self.pos;

        let mut fields = Vec::new();
        for f in spec.fields.clone() {
            let (field, ss_opt) = self.parse_field(&spec.structs, &ns, &f)?;
            if let Some(id) = f.id {
                if let Some(ss) = ss_opt {
                    ns.insert_struct(id, ss);
                } else if let StructFieldKind::Prim(ptr) = &field.kind {
                    ns.insert_pointer(id, ptr.clone());
                }
            }
            fields.push((f.id.clone(), field));
        }

        let size = self.pos - start;

        Ok((Struct { size, fields }, ns))
    }

    fn convert_prim(
        &mut self,
        apty: &ast::PrimType,
        ns: &sym::Namespace,
    ) -> SResult<PrimType> {
        Ok(match apty {
            ast::PrimType::Signed(len) => {
                PrimType::Signed(self.eval(&len, ns)? as u8)
            }
            ast::PrimType::Unsigned(len) => {
                PrimType::Unsigned(self.eval(&len, ns)? as u8)
            }
            ast::PrimType::Float(exponent, mantissa) => PrimType::Float(
                self.eval(&exponent, ns)? as u8,
                self.eval(&mantissa, ns)? as u8,
            ),
            ast::PrimType::BitVec(len) => {
                PrimType::BitVec(self.eval(&len, ns)? as u8)
            }
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
        specs: &HashMap<sym::Sym, ast::Struct>,
        ns: &sym::Namespace,
        ty: &ast::FieldType,
    ) -> SResult<(StructFieldKind, Option<sym::Namespace>)> {
        Ok(match &ty.kind {
            ast::FieldKind::Prim(apty) => {
                let spty = self.convert_prim(&apty, ns)?;
                let al = ty.alignment as u64 * 8;
                let start = if al == 0 || self.pos % al == 0 {
                    self.pos
                } else {
                    self.pos + (al - self.pos % al)
                };
                let ptr = Ptr {
                    start,
                    pty: spty,
                    byte_order: ty.byte_order,
                };
                self.parse_prim(&ptr)?;
                (StructFieldKind::Prim(ptr), None)
            }
            ast::FieldKind::Array(ty, count) => (
                StructFieldKind::Array(
                    self.parse_array(specs, ns, ty, count)?,
                ),
                None,
            ),
            ast::FieldKind::Struct(id, params) => {
                let mut args = Vec::new();
                for p in params {
                    args.push(self.eval(p, ns)?);
                }
                let struct_spec = specs
                    .get(&id)
                    .ok_or(self.err(SErrorKind::StructNotInScope(*id)))?;
                let (st, ss) = self.parse_struct(struct_spec, &args)?;
                (StructFieldKind::Struct(st), Some(ss))
            }
        })
    }

    fn parse_field(
        &mut self,
        specs: &HashMap<sym::Sym, ast::Struct>,
        ns: &sym::Namespace,
        field: &ast::Field,
    ) -> SResult<(StructField, Option<sym::Namespace>)> {
        self.span = field.span;
        let (kind, ss) = self.parse_field_kind(specs, ns, &field.ty)?;

        // TODO check constraints

        Ok((StructField { kind }, ss))
    }

    fn err(&self, kind: SErrorKind) -> SError {
        SError {
            span: self.span,
            pos: self.pos,
            kind,
        }
    }

    pub fn consume(self) -> R {
        self.f
    }
}
