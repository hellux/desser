use std::io::{BufRead, Error, ErrorKind, Seek, SeekFrom};

use crate::spec::ast;
use crate::structure::*;
use crate::sym;

#[derive(Debug)]
pub enum SError {
    EndOfFile,
    UnfulfilledConstraint,
    ExcessData,
    InvalidType,
    VariableNotInScope,
    IO(std::io::Error),
}

impl From<std::io::Error> for SError {
    fn from(e: std::io::Error) -> Self {
        SError::IO(e)
    }
}

impl From<std::io::ErrorKind> for SError {
    fn from(kind: std::io::ErrorKind) -> Self {
        SError::from(std::io::Error::from(kind))
    }
}

pub type SResult<T> = Result<T, SError>;

pub struct FileParser<R> {
    f: R,
    spec: ast::FileSpecification,
    pos: u64,
    length: u64,
}

impl<R: BufRead + Seek> FileParser<R> {
    pub fn new(mut f: R, spec: ast::FileSpecification) -> SResult<Self> {
        let length = f.seek(SeekFrom::End(0))? * 8;
        Ok(FileParser {
            f,
            spec: spec.clone(),
            pos: 0,
            length,
        })
    }

    pub fn parse(&mut self, root_sym: sym::Sym) -> SResult<StructuredFile> {
        let (root, _) = self.parse_struct(root_sym, &vec![])?;

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
            Err(SError::EndOfFile)
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
        let var = ns.get(id).unwrap();
        Ok(match var {
            sym::Variable::Direct(val) => *val,
            sym::Variable::Indirect(ptr) => ptr.eval_size(&mut self.f),
        })
    }

    fn parse_prim(
        &mut self,
        pty: &PrimType,
        ns: &sym::Namespace,
    ) -> SResult<()> {
        self.seek(SeekFrom::Current(pty.size() as i64))?;
        Ok(())
    }

    fn parse_array(
        &mut self,
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
            let (member, _) = self.parse_field_kind(ns, kind)?;
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
        params: &Vec<Val>,
    ) -> SResult<(Struct, sym::Namespace)> {
        let spec = self.spec.structs.get(&id).ok_or(Error::new(
            ErrorKind::NotFound,
            format!("Struct {} not declared.", id),
        ))?;

        let mut ns = sym::Namespace::new();

        for (pval, pname) in params.iter().zip(spec.parameters.iter()) {
            ns.insert_value(pname.clone(), *pval);
        }

        let start = self.pos;

        let mut fields = Vec::new();
        for f in spec.fields.clone() {
            let (field, ss_opt) = self.parse_field(&ns, &f)?;
            if let Some(id) = f.id {
                if let Some(ss) = ss_opt {
                    ns.insert_struct(id, ss);
                } else if let StructFieldKind::Prim(pty) = &field.kind {
                    let ptr = Ptr {
                        start: field.start,
                        pty: *pty,
                        byte_order: Order::LittleEndian,
                    };
                    ns.insert_pointer(id, ptr);
                }
            }
            fields.push((f.id.clone(), field));
        }

        let size = self.pos - start;

        Ok((
            Struct {
                start,
                size,
                fields,
            },
            ns,
        ))
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
        ns: &sym::Namespace,
        kind: &ast::FieldType,
    ) -> SResult<(StructFieldKind, Option<sym::Namespace>)> {
        Ok(match kind {
            ast::FieldType::Prim(apty) => {
                let spty = self.convert_prim(apty, ns)?;
                self.parse_prim(&spty, ns)?;
                (StructFieldKind::Prim(spty), None)
            }
            ast::FieldType::Array(kind, count) => (
                StructFieldKind::Array(self.parse_array(ns, kind, count)?),
                None,
            ),
            ast::FieldType::Struct(id, params) => {
                let mut args = Vec::new();
                for p in params {
                    args.push(self.eval(p, ns)?);
                }
                let (st, ss) = self.parse_struct(*id, &args)?;
                (StructFieldKind::Struct(st), Some(ss))
            }
        })
    }

    fn parse_field(
        &mut self,
        ns: &sym::Namespace,
        field: &ast::Field,
    ) -> SResult<(StructField, Option<sym::Namespace>)> {
        let start = self.pos;
        let (kind, ss) = self.parse_field_kind(ns, &field.ty)?;
        let size = self.pos - start;

        // TODO check constraints

        Ok((StructField { start, size, kind }, ss))
    }

    pub fn consume(self) -> (R, ast::FileSpecification) {
        (self.f, self.spec)
    }
}
