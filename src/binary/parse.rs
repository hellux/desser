use std::io::SeekFrom;
use std::rc::Rc;

use crate::{AddrBase, BuiltInIdent, Error, Span, SymbolTable};

use super::error::{EErrorKind, EResult, SError, SErrorKind, SResult};
use super::eval::{Eval, IntVal, Val};
use super::scope::{Name, Namespace, Scopes};
use super::*;

pub(super) fn parse<SR: SeekRead>(
    f: &mut SR,
    root_spec: Rc<ast::Struct>,
    symtab: &mut SymbolTable,
) -> Result<Struct, Error> {
    let length = ByteSize(f.seek(SeekFrom::End(0)).unwrap()).into();
    let scope = Scopes::new(length, symtab);
    let mut fp = FileParser::new(f, scope, length, symtab);
    let root_st = match fp.parse_struct(&root_spec) {
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
    scope: Scopes,
    traversed_fields: Vec<(Span, Option<Sym>, BitPos)>,
    symtab: &'s SymbolTable,
    self_sym: Sym,
}

impl<'s, SR: SeekRead> FileParser<'s, SR> {
    fn new(
        f: &'s mut SR,
        scope: Scopes,
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
            Val::Integer(i) => Err(expr.err(EErrorKind::NegativeSize(i))),
            _ => Err(expr.err(EErrorKind::NonIntegerSize)),
        }
    }

    fn eval_nonzero(&mut self, expr: &ast::Expr) -> EResult<bool> {
        Eval::new(self.f, &self.scope, &self.symtab).eval_nonzero(expr)
    }

    fn eval_access(&mut self, expr: &ast::Expr) -> SResult<Name> {
        Ok(Eval::new(self.f, &self.scope, &self.symtab).eval_access(expr)?)
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
            let fk = match self
                .parse_field_type(&arr.ty.properties, &arr.ty.kind)
            {
                Ok(fk) => fk,
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
                self.scope.enter_selfscope(fk.clone());
                let res = self.eval_nonzero(&fin);
                self.scope.exit_selfscope();

                final_signal = res?
            }

            if start.is_none() {
                start = Some(fk.start());
            }

            is.push(fk);
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

        let len = self.arr_len(&fl.arr)?;

        let mut start = None;
        for idx in 0..len {
            let elem = self.arr_elem(&fl.arr, idx)?;

            let mut ss = Namespace::new();
            ss.sym_insert(fl.elem, Name::Field(elem));

            self.scope.enter_subscope(ss);
            let fk_res = self.parse_field_type(&fl.ty.properties, &fl.ty.kind);
            self.scope.exit_subscope();

            let fk = fk_res?;

            if start.is_none() {
                start = Some(fk.start());
            }

            is.push(fk);
        }
        let start = start.unwrap_or(self.pos);
        let size = self.pos - start;

        Ok(Array {
            start,
            size,
            elements: is,
        })
    }

    fn parse_defcall(
        &mut self,
        def: &Rc<ast::Definition>,
        properties: &ast::Properties,
        actual_params: &[ast::Expr],
    ) -> SResult<Rc<FieldKind>> {
        if def.formal_params.len() != actual_params.len() {
            return Err(SErrorKind::FormalActualMismatch(
                def.formal_params.len(),
                actual_params.len(),
            ));
        }

        let mut static_space = Namespace::new();
        for (sym, expr) in def.formal_params.iter().zip(actual_params.iter()) {
            let name = self.eval_access(expr)?;
            let exists = static_space.sym_insert(*sym, name);
            if exists {
                return Err(SErrorKind::FieldExists(*sym));
            }
        }

        self.scope.enter_scope(self.pos, static_space);
        let ft_res = self.parse_field_type(
            &def.ty.properties.apply(properties),
            &def.ty.kind,
        );
        self.scope.exit_scope();

        ft_res
    }

    fn parse_struct(&mut self, spec: &ast::Struct) -> SResult<Struct> {
        let mut static_space = Namespace::new();
        for (sym, expr) in &spec.header.constants {
            let name = self.eval_access(expr)?;
            let exists = static_space.sym_insert(*sym, name);
            if exists {
                return Err(SErrorKind::FieldExists(*sym));
            }
        }
        for (sym, st) in &spec.header.defs {
            let name = Name::Def(st.clone());
            let exists = static_space.sym_insert(*sym, name);
            if exists {
                return Err(SErrorKind::FieldExists(*sym));
            }
        }

        self.scope.enter_scope(self.pos, static_space);
        let success = self.parse_block(&spec.block);
        let st = self.scope.exit_scope();

        success?;

        Ok(st)
    }

    fn parse_block(&mut self, block: &[ast::Stmt]) -> SResult<()> {
        block.iter().try_for_each(|s| self.parse_statement(s))
    }

    fn parse_statement(&mut self, stmt: &ast::Stmt) -> SResult<()> {
        match stmt {
            ast::Stmt::Field(f) => self.parse_field(&f)?,
            ast::Stmt::Let(sym, expr) => {
                let name = self.eval_access(expr)?;
                self.scope.insert_local(*sym, name);
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
                    match self.eval_access(expr)? {
                        Name::Field(fk) => {
                            view::view_structure(
                                self.f,
                                &mut std::io::stderr(),
                                &fk,
                                self.symtab,
                            )
                            .ok();
                        }
                        Name::Def(_) => {
                            eprintln!("<definition>")
                        }
                        Name::Value(val) => eprintln!("{:?} ", val),
                    }
                }
                eprintln!();
            }
        }

        Ok(())
    }

    fn parse_field_type(
        &mut self,
        properties: &ast::Properties,
        kind: &ast::FieldKind,
    ) -> SResult<Rc<FieldKind>> {
        self.seek_loc(&properties.loc)?;
        self.align(&properties.alignment)?;

        let fk = match kind {
            ast::FieldKind::Array(arr) => {
                Rc::new(FieldKind::Array(match arr {
                    ast::Array::Std(arr) => self.parse_std_array(arr)?,
                    ast::Array::For(arr) => self.parse_for_array(arr)?,
                }))
            }
            ast::FieldKind::Struct(st) => {
                Rc::new(FieldKind::Struct(self.parse_struct(st)?))
            }
            ast::FieldKind::Block(block) => {
                self.scope.enter_subblock(self.pos);
                let success = self.parse_block(block);
                let st = self.scope.exit_subblock();
                success?;
                Rc::new(FieldKind::Struct(st))
            }
            ast::FieldKind::Name(def_sym, args) => {
                let name = self
                    .scope
                    .get(def_sym.sym)
                    .ok_or(SErrorKind::TypeNotFound(*def_sym))?;
                if let Name::Def(def) = name {
                    self.parse_defcall(&def, properties, &args)?
                } else {
                    return Err(SErrorKind::NonType(*def_sym));
                }
            }
            ast::FieldKind::Prim(apty) => {
                let spty = self.convert_prim(&apty)?;
                let ptr = Ptr {
                    start: self.pos,
                    pty: spty,
                    order: properties.order.unwrap_or_default(),
                };
                self.seek(ptr.start + ptr.pty.size())?;
                Rc::new(FieldKind::Prim(ptr))
            }
            ast::FieldKind::If(if_type) => {
                if self.eval_nonzero(&if_type.cond)? {
                    self.parse_field_type(
                        &if_type.if_res.properties,
                        &if_type.if_res.kind,
                    )?
                } else {
                    self.parse_field_type(
                        &if_type.else_res.properties,
                        &if_type.else_res.kind,
                    )?
                }
            }
            ast::FieldKind::Null => Rc::new(FieldKind::Null(self.pos)),
        };

        for constraint in &properties.constraints {
            self.scope.enter_selfscope(fk.clone());
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

        let prev_pos = self.pos;
        let fk_res =
            self.parse_field_type(&field.ty.properties, &field.ty.kind);

        let fk = if field.ty.properties.peek {
            self.pos = prev_pos;
            fk_res.unwrap_or(Rc::new(FieldKind::Null(prev_pos)))
            // field might have been elsewhere depending on props.location
        } else {
            fk_res?
        };

        let named = !field.hidden;
        let hidden = field.ty.properties.peek;
        let exists = self.scope.insert_field(field.id, fk, named, hidden);
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

    fn arr_len(&mut self, expr: &ast::Expr) -> SResult<usize> {
        if let Name::Field(rc) = self.eval_access(expr)? {
            if let FieldKind::Array(arr) = rc.as_ref() {
                Ok(arr.elements.len())
            } else {
                Err(expr.err(EErrorKind::NonArrayIterator).into())
            }
        } else {
            Err(expr.err(EErrorKind::NonArrayIterator).into())
        }
    }

    fn arr_elem(
        &mut self,
        expr: &ast::Expr,
        idx: usize,
    ) -> SResult<Rc<FieldKind>> {
        if let Name::Field(rc) = self.eval_access(expr)? {
            if let FieldKind::Array(arr) = rc.as_ref() {
                Ok(arr.elements.get(idx).unwrap().clone())
            } else {
                Err(expr.err(EErrorKind::NonArrayIterator).into())
            }
        } else {
            Err(expr.err(EErrorKind::NonArrayIterator).into())
        }
    }
}
