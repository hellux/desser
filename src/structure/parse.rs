use std::io::{BufRead, Seek, SeekFrom};

use super::error::{SError, SErrorKind, SResult};
use super::eval::{IntVal, Partial, Val};
use super::scope::{IndexSpace, Name, NameArray, Namespace, Scope};
use super::*;
use crate::{AddrBase, Error, Span, SymbolTable};

pub fn parse<'s, R: BufRead + Seek>(
    f: &'s mut R,
    root_spec: &'s ast::Struct,
    symtab: &mut SymbolTable,
) -> Result<Name<'s>, Error> {
    let length = f.seek(SeekFrom::End(0)).unwrap() * 8;
    let scope = Scope::new(length, symtab);
    let mut fp = FileParser::new(f, scope, length);
    let root_name = match fp.parse_struct(root_spec, &[]) {
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

    Ok(root_name)
}

struct FileParser<'s, R> {
    f: &'s mut R,
    span: Span,
    pos: u64,
    length: u64,
    scope: Scope<'s>,
}

impl<'s, R: BufRead + Seek> FileParser<'s, R> {
    fn new(f: &'s mut R, scope: Scope<'s>, length: u64) -> Self {
        FileParser {
            f,
            span: Span(0, 0),
            pos: 0,
            length,
            scope,
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
        //let base = self.scope.base();
        match &loc.expr {
            Some(expr) => {
                let base = match loc.base {
                    AddrBase::Absolute => 0,
                    AddrBase::Relative => self.pos,
                    AddrBase::Local => 0, // FIXME
                };

                let offset = self.eval_size(expr)? as u64
                    * if loc.bitwise { 1 } else { 8 };
                self.seek(base + offset)?;
            }
            None => {}
        };

        Ok(())
    }

    fn align(&mut self, alignment: &ast::Alignment) -> SResult<()> {
        match &alignment.expr {
            Some(expr) => {
                let al = self.eval_size(expr)?;
                if al > 0 {
                    let al = al as u64 * if alignment.bitwise { 1 } else { 8 };
                    if self.pos % al > 0 {
                        self.pos += al - self.pos % al
                    }
                } else {
                    return Err(SErrorKind::InvalidValue(al as u64));
                }
            }
            None => {}
        };

        Ok(())
    }

    fn eval(&mut self, expr: &ast::Expr) -> SResult<Val> {
        eval::eval(expr, self.f, &self.scope)
    }

    fn eval_size(&mut self, expr: &ast::Expr) -> SResult<IntVal> {
        match self.eval(expr)? {
            Val::Integer(size) if size >= 0 => Ok(size),
            Val::Integer(_neg) => Err(SErrorKind::NegativeSize),
            _ => Err(SErrorKind::InvalidType),
        }
    }

    fn eval_bool(&mut self, expr: &ast::Expr) -> SResult<bool> {
        match self.eval(expr)? {
            Val::Integer(0) => Ok(false),
            Val::Integer(_nz) => Ok(true),
            _ => Err(SErrorKind::InvalidType),
        }
    }

    fn eval_partial(&mut self, expr: &ast::Expr) -> SResult<Partial<'s>> {
        let part = eval::eval_partial(expr, self.f, &self.scope)?;
        Ok(unsafe { std::mem::transmute::<_, Partial<'s>>(part) })
    }

    fn parse_std_array(&mut self, arr: &ast::StdArray) -> SResult<Name<'s>> {
        let (min_size, max_size) = match &arr.size {
            ast::ArraySize::Exactly(n) => {
                let n = self.eval_size(n)?;
                (n, Some(n))
            }
            ast::ArraySize::Within(a, b) => {
                (self.eval_size(a)?, Some(self.eval_size(b)?))
            }
            ast::ArraySize::AtLeast(n) => (self.eval_size(n)?, None),
        };

        let mut start = None;
        let mut is = IndexSpace::new();
        loop {
            if let Some(m) = max_size {
                if is.len() >= m as usize {
                    break;
                }
            }

            let elem_start = self.pos;

            // parse until constraint fails or max num is reached
            let name = match self.parse_field_type(&arr.ty) {
                Ok(name) => name,
                Err(k) => match k {
                    SErrorKind::FailedConstraint
                    | SErrorKind::EndOfFile(_)
                        if is.len() >= min_size as usize =>
                    {
                        self.seek(elem_start)?;
                        break;
                    }
                    _ => return Err(k),
                },
            };

            if start.is_none() {
                start = name.start();
            }

            is.push(name);
        }
        let start = start.unwrap_or(self.pos);
        let size = self.pos - start;

        Ok(Name::Array(NameArray {
            start,
            size,
            elements: is,
        }))
    }

    fn parse_for_array(&mut self, fl: &ast::ForArray) -> SResult<Name<'s>> {
        let mut is = IndexSpace::new();

        let len = self.eval_partial(&fl.arr)?.name()?.elements()?.len();

        let mut start = None;
        for idx in 0..len {
            let elem = self.eval_partial(&fl.arr)?.name()?.get_element(idx)?;

            let mut ss = Namespace::new();
            ss.insert(fl.elem, Name::Reference(elem));

            self.scope.enter_subscope(ss);
            let name_res = self.parse_field_type(&fl.ty);
            self.scope.exit_subscope();

            let name = name_res?;

            if start.is_none() {
                start = name.start();
            }

            is.push(name);
        }
        let start = start.unwrap_or(self.pos);
        let size = self.pos - start;

        Ok(Name::Array(NameArray {
            start,
            size,
            elements: is,
        }))
    }

    fn parse_struct(
        &mut self,
        spec: &'s ast::Struct,
        params: &[ast::Expr],
    ) -> SResult<Name<'s>> {
        if spec.formal_params.len() != params.len() {
            return Err(SErrorKind::FormalActualMismatch);
        }

        let mut static_space = Namespace::new();
        for (sym, expr) in spec.formal_params.iter().zip(params.iter()) {
            let name = match self.eval_partial(expr)? {
                Partial::Value(val) => Name::Value(val),
                Partial::Name(name) => Name::Reference(name),
            };
            static_space.insert(*sym, name);
        }
        for (sym, expr) in &spec.constants {
            let name = match self.eval_partial(expr)? {
                Partial::Value(val) => Name::Value(val),
                Partial::Name(name) => Name::Reference(name),
            };
            static_space.insert(*sym, name);
        }
        for (sym, spec) in &spec.structs {
            let name = Name::Spec(spec);
            static_space.insert(*sym, name);
        }

        self.scope.enter_struct(self.pos, static_space);

        let success = self.parse_block(&spec.block);
        let nst = self.scope.exit_struct();

        success?;

        Ok(Name::Struct(nst))
    }

    fn parse_block(&mut self, block: &[ast::Stmt]) -> SResult<()> {
        for s in block {
            match s {
                ast::Stmt::Field(f) => self.parse_field(&f)?,
                ast::Stmt::If(if_stmt) => {
                    let body = if self.eval_bool(&if_stmt.cond)? {
                        &if_stmt.if_body
                    } else {
                        let mut i = 0;
                        loop {
                            if let Some((c, b)) = if_stmt.elseifs.get(i) {
                                if self.eval_bool(c)? {
                                    break b;
                                } else {
                                    i += 1;
                                }
                            } else {
                                break &if_stmt.else_body;
                            }
                        }
                    };

                    self.parse_block(body)?;
                }
                ast::Stmt::Constrain(exprs) => {
                    for expr in exprs {
                        if !self.eval_bool(expr)? {
                            return Err(SErrorKind::FailedConstraint);
                        }
                    }
                }
                ast::Stmt::Debug(exprs) => {
                    eprint!("debug: ");
                    for expr in exprs {
                        eprint!("{:?} ", self.eval(expr)?);
                    }
                    eprintln!();
                }
            }
        }

        Ok(())
    }

    fn parse_field_type(&mut self, ty: &ast::FieldType) -> SResult<Name<'s>> {
        self.seek_loc(&ty.loc)?;
        self.align(&ty.alignment)?;

        let mut name = match &ty.kind {
            ast::FieldKind::Array(arr) => match arr {
                ast::Array::Std(arr) => self.parse_std_array(arr)?,
                ast::Array::For(arr) => self.parse_for_array(arr)?,
            },
            ast::FieldKind::Block(block) => {
                self.scope.enter_subblock(self.pos);
                let success = self.parse_block(block);
                let nst = self.scope.exit_subblock();
                success?;
                Name::Struct(nst)
            }
            ast::FieldKind::Struct(spec_sym, args) => {
                let name = self.scope.get(*spec_sym)?;
                if let Name::Spec(spec) = name {
                    self.parse_struct(spec, &args)?
                } else {
                    return Err(SErrorKind::InvalidType);
                }
            }
            ast::FieldKind::Prim(apty) => {
                let spty = self.convert_prim(&apty)?;
                let ptr = Ptr {
                    start: self.pos,
                    pty: spty,
                    byte_order: ty.byte_order,
                };
                self.seek(ptr.start + ptr.pty.size() as u64)?;
                Name::Field(ptr)
            }
        };

        if let Some(constraint) = &ty.constraint {
            self.scope.enter_selfscope(name);
            let res = self.eval_bool(constraint);
            name = self.scope.exit_selfscope();

            if !res? {
                return Err(SErrorKind::FailedConstraint);
            }
        }

        Ok(name)
    }

    fn parse_field(&mut self, field: &ast::Field) -> SResult<()> {
        self.span = field.span;

        let name = self.parse_field_type(&field.ty)?;
        if !field.hidden {
            self.scope.insert_field(field.id, name);
        }

        Ok(())
    }

    fn convert_prim(&mut self, apty: &ast::PrimType) -> SResult<PrimType> {
        Ok(match apty {
            //        ast::PrimType::Signed(len) => {
            //            PrimType::Signed(self.eval(&len)? as u8)
            //        }
            //        ast::PrimType::Unsigned(len) => {
            //            PrimType::Unsigned(self.eval(&len)? as u8)
            //        }
            //        ast::PrimType::Float(exponent, mantissa) => PrimType::Float(
            //            self.eval(&exponent)? as u8,
            //            self.eval(&mantissa)? as u8,
            //        ),
            ast::PrimType::BitVec(len) => {
                PrimType::BitVec(self.eval_size(&len)? as u8)
            }
            ast::PrimType::Char => PrimType::Char,
            ast::PrimType::U8 => PrimType::U8,
            ast::PrimType::I8 => PrimType::I8,
            ast::PrimType::U16 => PrimType::U16,
            ast::PrimType::I16 => PrimType::I16,
            ast::PrimType::U32 => PrimType::U32,
            ast::PrimType::I32 => PrimType::I32,
            ast::PrimType::U64 => PrimType::U64,
            ast::PrimType::I64 => PrimType::I64,
            ast::PrimType::F32 => PrimType::F32,
            ast::PrimType::F64 => PrimType::F64,
        })
    }
}
