use std::io::{BufRead, Seek, SeekFrom};

use super::error::{EErrorKind, EResult, SError, SErrorKind, SResult};
use super::eval::{Eval, IntVal, Partial, Val};
use super::scope::{Name, Namespace, Scope};
use super::*;
use crate::{AddrBase, BuiltInIdent, Error, Span, SymbolTable};

pub(super) fn parse<'s, R: BufRead + Seek>(
    f: &'s mut R,
    root_spec: &'s ast::Struct,
    symtab: &mut SymbolTable,
) -> Result<Struct, Error> {
    let length = ByteSize(f.seek(SeekFrom::End(0)).unwrap()).into();
    let scope = Scope::new(length, symtab);
    let mut fp = FileParser::new(f, scope, length, symtab);
    let root_st = match fp.parse_struct(root_spec, &[]) {
        Ok(r) => r,
        Err(kind) => {
            let e = SError {
                backtrace: fp.traversed_fields,
                pos: fp.pos,
                kind,
            };
            return Err(Error::from(e, symtab));
        }
    };

    Ok(root_st)
}

struct FileParser<'s, R> {
    f: &'s mut R,
    pos: BitPos,
    length: BitSize,
    scope: Scope<'s>,
    traversed_fields: Vec<(Span, Option<Sym>, BitPos)>,
    symtab: &'s SymbolTable,
    self_sym: Sym,
}

impl<'s, R: BufRead + Seek> FileParser<'s, R> {
    fn new(
        f: &'s mut R,
        scope: Scope<'s>,
        length: BitSize,
        symtab: &'s SymbolTable,
    ) -> Self {
        let self_sym = symtab.ident_sym(BuiltInIdent::IdSelf);
        FileParser {
            f,
            traversed_fields: Vec::new(),
            pos: BitPos::origin(),
            length,
            scope,
            symtab,
            self_sym,
        }
    }

    fn seek(&mut self, pos: BitPos) -> SResult<()> {
        self.pos = pos;
        if self.pos <= self.length {
            Ok(())
        } else {
            Err(SErrorKind::EndOfFile(self.length))
        }
    }

    fn seek_loc(&mut self, loc: &ast::Location) -> SResult<()> {
        match &loc.expr {
            Some(expr) => {
                let base = match loc.base {
                    AddrBase::Absolute => BitPos::new(0),
                    AddrBase::Relative => self.pos,
                    AddrBase::Local => self.scope.base(),
                };

                let val = self.eval_size(expr)? as u64;
                let offset = if loc.bitwise {
                    BitSize::new(val)
                } else {
                    ByteSize(val).into()
                };

                let new_pos = base + offset;
                if new_pos >= self.pos {
                    self.seek(base + offset)?;
                } else {
                    return Err(SErrorKind::AddrBeforePos(new_pos));
                }
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
                    let al_bs = if alignment.bitwise {
                        BitSize::new(al as u64)
                    } else {
                        ByteSize(al as u64).into()
                    };
                    self.pos = self.pos.align(al_bs);
                } else {
                    return Err(SErrorKind::NonPositiveAlignment(al));
                }
            }
            None => {}
        };

        Ok(())
    }

    fn eval_size(&mut self, expr: &ast::Expr) -> EResult<IntVal> {
        let val = Eval::new(self.f, &self.scope, &self.symtab).eval(expr)?;
        match val {
            Val::Integer(size) if size >= 0 => Ok(size),
            Val::Integer(_neg) => Err(expr.err(EErrorKind::NegativeSize)),
            _ => Err(expr.err(EErrorKind::NonIntegerSize)),
        }
    }

    fn eval_nonzero(&mut self, expr: &ast::Expr) -> EResult<bool> {
        Eval::new(self.f, &self.scope, &self.symtab).eval_nonzero(expr)
    }

    fn eval_partial(&mut self, expr: &ast::Expr) -> SResult<Partial<'s>> {
        let part =
            Eval::new(self.f, &self.scope, &self.symtab).eval_partial(expr)?;
        // name is never removed and namespace outlives 's
        Ok(unsafe { std::mem::transmute::<_, Partial<'s>>(part) })
    }

    fn parse_std_array(&mut self, arr: &ast::StdArray) -> SResult<Array> {
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
        let mut is = Vec::new();
        let mut final_signal = false;
        while !final_signal {
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
                    SErrorKind::FailedConstraint(_)
                    | SErrorKind::EndOfFile(_)
                        if is.len() >= min_size as usize =>
                    {
                        self.seek(elem_start)?;
                        break;
                    }
                    _ => return Err(k),
                },
            };

            if let Some(fin) = &arr.ty.properties.fin {
                self.scope.enter_selfscope(unsafe {
                    std::mem::transmute::<&FieldKind, &'s FieldKind>(&name)
                });
                let res = self.eval_nonzero(&fin);
                self.scope.exit_selfscope();

                final_signal = res?
            }

            if start.is_none() {
                start = Some(name.start());
            }

            is.push(name);
        }
        let start = start.unwrap_or(self.pos);
        let size = self.pos - start;

        Ok(Array {
            start,
            size,
            elements: is,
        })
    }

    fn parse_for_array(&mut self, fl: &ast::ForArray) -> SResult<Array> {
        let mut is = Vec::new();

        let arr = if let Partial::Name(Name::Field(FieldKind::Array(arr))) =
            self.eval_partial(&fl.arr)?
        {
            arr
        } else {
            return Err(fl.arr.err(EErrorKind::NonArrayIterator).into());
        };

        let len = arr.elements.len();

        let mut start = None;
        for idx in 0..len {
            let elem = arr.elements.get(idx).unwrap();

            let mut ss = Namespace::new();
            ss.insert(Some(fl.elem), Name::Field(elem));

            self.scope.enter_subscope(ss);
            let name_res = self.parse_field_type(&fl.ty);
            self.scope.exit_subscope();

            let name = name_res?;

            if start.is_none() {
                start = Some(name.start());
            }

            is.push(name);
        }
        let start = start.unwrap_or(self.pos);
        let size = self.pos - start;

        Ok(Array {
            start,
            size,
            elements: is,
        })
    }

    fn parse_struct(
        &mut self,
        spec: &'s ast::Struct,
        params: &[ast::Expr],
    ) -> SResult<Struct> {
        if spec.formal_params.len() != params.len() {
            return Err(SErrorKind::FormalActualMismatch(
                spec.formal_params.len(),
                params.len(),
            ));
        }

        let mut static_space = Namespace::new();
        for (sym, expr) in spec.formal_params.iter().zip(params.iter()) {
            let exists =
                static_space.insert_partial(*sym, self.eval_partial(expr)?);
            if exists {
                return Err(SErrorKind::FieldExists(*sym));
            }
        }
        for (sym, expr) in &spec.constants {
            let exists =
                static_space.insert_partial(*sym, self.eval_partial(expr)?);
            if exists {
                return Err(SErrorKind::FieldExists(*sym));
            }
        }
        for (sym, spec) in &spec.structs {
            let exists = static_space.insert(Some(*sym), Name::Spec(spec));
            if exists {
                return Err(SErrorKind::FieldExists(*sym));
            }
        }

        self.scope.enter_struct(self.pos, static_space);
        let success = self.parse_block(&spec.block);
        let st = self.scope.exit_struct();

        success?;

        Ok(st)
    }

    fn parse_block(&mut self, block: &[ast::Stmt]) -> SResult<()> {
        block.iter().map(|s| self.parse_statement(s)).collect()
    }

    fn parse_statement(&mut self, stmt: &ast::Stmt) -> SResult<()> {
        match stmt {
            ast::Stmt::Field(f) => self.parse_field(&f)?,
            ast::Stmt::Let(sym, expr) => {
                let part = self.eval_partial(expr)?;
                self.scope.insert_local(*sym, part);
            }
            ast::Stmt::Constrain(exprs) => {
                for expr in exprs {
                    if !self.eval_nonzero(expr)? {
                        return Err(SErrorKind::FailedConstraint(expr.span));
                    }
                }
            }
            ast::Stmt::Debug(exprs) => {
                for expr in exprs {
                    match self.eval_partial(expr)? {
                        Partial::Name(name) => match name {
                            Name::Field(fk) => {
                                view::view_structure(
                                    self.f,
                                    &mut std::io::stderr(),
                                    &fk,
                                    self.symtab,
                                )
                                .ok();
                            }
                            Name::Spec(_) => {
                                eprintln!("<struct specification>")
                            }
                            Name::Value(val) => eprintln!("{:?} ", val),
                        },
                        Partial::Value(val) => eprintln!("{:?} ", val),
                    }
                }
                eprintln!();
            }
        }

        Ok(())
    }

    fn parse_field_type(&mut self, ty: &ast::FieldType) -> SResult<FieldKind> {
        self.seek_loc(&ty.properties.loc)?;
        self.align(&ty.properties.alignment)?;

        let fk = match &ty.kind {
            ast::FieldKind::Array(arr) => FieldKind::Array(match arr {
                ast::Array::Std(arr) => self.parse_std_array(arr)?,
                ast::Array::For(arr) => self.parse_for_array(arr)?,
            }),
            ast::FieldKind::Block(block) => {
                self.scope.enter_subblock(self.pos);
                let success = self.parse_block(block);
                let st = self.scope.exit_subblock();
                success?;
                FieldKind::Struct(st)
            }
            ast::FieldKind::Struct(spec_sym, args) => {
                let name = self
                    .scope
                    .get(spec_sym.sym)
                    .ok_or(SErrorKind::TypeNotFound(*spec_sym))?;
                if let Name::Spec(spec) = name {
                    FieldKind::Struct(self.parse_struct(spec, &args)?)
                } else {
                    return Err(SErrorKind::NonSpec(*spec_sym));
                }
            }
            ast::FieldKind::Prim(apty) => {
                let spty = self.convert_prim(&apty)?;
                let ptr = Ptr {
                    start: self.pos,
                    pty: spty,
                    order: ty.properties.order,
                };
                self.seek(ptr.start + ptr.pty.size())?;
                FieldKind::Prim(ptr)
            }
            ast::FieldKind::If(if_type) => {
                if self.eval_nonzero(&if_type.cond)? {
                    self.parse_field_type(&if_type.if_res)?
                } else {
                    self.parse_field_type(&if_type.else_res)?
                }
            }
            ast::FieldKind::Null => FieldKind::Null(self.pos),
        };

        for constraint in &ty.properties.constraints {
            self.scope.enter_selfscope(unsafe {
                std::mem::transmute::<&FieldKind, &'s FieldKind>(&fk)
            });
            let self_var = ast::Expr {
                kind: ast::ExprKind::Variable(self.self_sym),
                span: self.traversed_fields.last().unwrap().0,
            };
            let (res, span) = match constraint {
                ast::Constraint::Generic(constr) => {
                    (self.eval_nonzero(constr), constr.span)
                }
                ast::Constraint::Binary(op, expr) => {
                    let cmp = ast::Expr {
                        kind: ast::ExprKind::Binary(
                            *op,
                            Box::new(self_var),
                            Box::new(expr.clone()),
                        ),
                        span: expr.span,
                    };
                    (self.eval_nonzero(&cmp), cmp.span)
                }
                ast::Constraint::Zero(z) => (
                    self.eval_nonzero(&self_var).map(|r| r ^ z),
                    self_var.span,
                ),
            };
            self.scope.exit_selfscope();

            if !res? {
                return Err(SErrorKind::FailedConstraint(span));
            }
        }

        Ok(fk)
    }

    fn parse_field(&mut self, field: &ast::Field) -> SResult<()> {
        self.traversed_fields.push((field.span, field.id, self.pos));

        let fk = self.parse_field_type(&field.ty)?;

        let exists = self.scope.insert_field(field.id, fk, field.hidden);
        if exists {
            return Err(SErrorKind::FieldExists(field.id.unwrap()));
        }

        self.traversed_fields.pop();

        Ok(())
    }

    fn convert_prim(&mut self, apty: &ast::PrimType) -> SResult<PrimType> {
        Ok(match apty {
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
