use std::io::{BufRead, Error, ErrorKind, Seek, SeekFrom};

use crate::ast;
use crate::sym;
use crate::{PError, PResult};

#[derive(Debug)]
pub enum StructFieldKind {
    Prim(ast::PrimType),
    Array(Vec<StructFieldKind>),
    Struct(Struct),
}

#[derive(Debug)]
pub struct StructField {
    pub start: u64,
    pub size: u64,
    pub kind: StructFieldKind,
}

#[derive(Debug)]
pub struct Struct {
    pub start: u64,
    pub size: u64,
    pub fields: Vec<(Option<sym::Sym>, StructField)>,
}

#[derive(Debug)]
pub struct StructuredFile {
    pub size: u64,
    pub root: Struct,
}

pub struct FileParser<R> {
    f: R,
    dsr: ast::FileSpecification,
    pos: u64,
    length: u64,
}

impl<R: BufRead + Seek> FileParser<R> {
    pub fn new(mut f: R, dsr: ast::FileSpecification) -> PResult<Self> {
        let length = f.seek(SeekFrom::End(0))? * 8;
        Ok(FileParser {
            f,
            dsr: dsr.clone(),
            pos: 0,
            length,
        })
    }

    pub fn parse(&mut self) -> PResult<StructuredFile> {
        let root = self.parse_struct(0, &vec![])?;

        Ok(StructuredFile {
            size: self.length,
            root,
        })
    }

    fn seek(&mut self, from: SeekFrom) -> PResult<u64> {
        let new_pos = match from {
            SeekFrom::Start(addr) => addr,
            SeekFrom::Current(rel_addr) => (self.pos as i64 + rel_addr) as u64,
            SeekFrom::End(rel_addr) => (self.length as i64 + rel_addr) as u64,
        };

        if new_pos < self.length {
            self.pos = new_pos;
            Ok(self.pos)
        } else {
            Err(PError::EndOfFile)
        }
    }

    pub fn eval(&self, expr: &ast::Expr, ns: &sym::Namespace) -> PResult<i64> {
        match expr {
            ast::Expr::Int(val) => Ok(*val),
            ast::Expr::Identifier(id) => self.eval_id(id, ns),
            ast::Expr::Binary(binop) => {
                let left = self.eval(&binop.left, ns)?;
                let right = self.eval(&binop.right, ns)?;
                println!("{}, {}", left, right);
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

    fn eval_id(&self, id: &sym::Sym, ns: &sym::Namespace) -> PResult<i64> {
        if let Some(val) = ns.get(id) {
            Ok(*val)
        } else if let Some(val) = self.dsr.constants.get(id) {
            Ok(*val)
        } else {
            Err(PError::VariableNotInScope)
        }
    }

    fn parse_value(
        &mut self,
        ty: &ast::PrimType,
        ns: &sym::Namespace,
    ) -> PResult<()> {
        let size = match ty {
            ast::PrimType::Signed(len) => self.eval(&len, ns)?,
            ast::PrimType::Unsigned(len) => self.eval(&len, ns)?,
            ast::PrimType::Float(exponent, mantissa) => {
                1 + self.eval(&exponent, ns)? + self.eval(&mantissa, ns)?
            }
            ast::PrimType::BitVec(len) => self.eval(&len, ns)?,
        } as i64;

        self.seek(SeekFrom::Current(size))?;

        Ok(())
    }

    fn parse_array(
        &mut self,
        ns: &sym::Namespace,
        kind: &ast::FieldType,
        size: &ast::ArraySize,
    ) -> PResult<Vec<StructFieldKind>> {
        let mut members = Vec::new();

        let max_size = match size {
            ast::ArraySize::Exactly(n) => Some(self.eval(n, ns)?),
            ast::ArraySize::Within(_, n) => Some(self.eval(n, ns)?),
            ast::ArraySize::AtLeast(_) => None,
        };

        let mut n = 0;
        loop {
            let member = self.parse_field_kind(ns, kind)?;
            members.push(member);

            // TODO stop on invalid member
            n += 1;
            if let Some(m) = max_size {
                if n >= m {
                    break;
                }
            }
        }

        Ok(members)
    }

    fn parse_struct(
        &mut self,
        id: sym::Sym,
        params: &Vec<i64>,
    ) -> PResult<Struct> {
        let spec = self.dsr.structs.get(&id).ok_or(Error::new(
            ErrorKind::NotFound,
            format!("Struct {} not declared.", id),
        ))?;

        let mut ns = sym::Namespace::new();

        for (pval, pname) in params.iter().zip(spec.parameters.iter()) {
            ns.insert(pname.clone(), *pval);
        }

        let start = self.pos;

        let mut fields = Vec::new();
        for field in spec.fields.clone() {
            fields.push((field.id.clone(), self.parse_field(&ns, &field)?));
            // TODO add to ns
        }

        let size = self.pos - start;

        Ok(Struct {
            start,
            size,
            fields,
        })
    }

    fn parse_field_kind(
        &mut self,
        ns: &sym::Namespace,
        kind: &ast::FieldType,
    ) -> PResult<StructFieldKind> {
        Ok(match kind {
            ast::FieldType::Prim(ty) => {
                self.parse_value(ty, ns)?;
                StructFieldKind::Prim(ty.clone())
            }
            ast::FieldType::Array(kind, count) => {
                StructFieldKind::Array(self.parse_array(ns, kind, count)?)
            }
            ast::FieldType::Struct(id, params) => {
                let mut args = Vec::new();
                for p in params {
                    args.push(self.eval(p, ns)?);
                }
                StructFieldKind::Struct(self.parse_struct(*id, &args)?)
            }
        })
    }

    fn parse_field(
        &mut self,
        ns: &sym::Namespace,
        field: &ast::Field,
    ) -> PResult<StructField> {
        let from = match &field.start {
            ast::Addr::Absolute(ofs) => {
                SeekFrom::Start(self.eval(ofs, ns)? as u64)
            }
            ast::Addr::Relative(ofs) => {
                SeekFrom::Current(self.eval(ofs, ns)? as i64)
            }
        };

        let start = self.seek(from)?;
        let kind = self.parse_field_kind(ns, &field.ty)?;
        let size = self.pos - start;

        // TODO check constraints

        Ok(StructField { start, size, kind })
    }
}
