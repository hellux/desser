use std::io::{Read, Seek};

use crate::spec::ast::{BinOp, Expr, ExprKind, UnOp};
use crate::spec::LitKind;
use crate::{BuiltInAttr, SpannedSym};

use super::error::{EError, EErrorKind, EResult};
use super::scope::{Name, Scope};
use super::*;

pub type IntVal = i64;
pub type FloatVal = f64;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Integer(IntVal),
    Float(FloatVal),
    Compound(Vec<u8>, BitSize),
}

impl Val {
    pub fn int(&self) -> Option<IntVal> {
        if let Val::Integer(val) = self {
            Some(*val)
        } else {
            None
        }
    }

    pub fn float(&self) -> Option<FloatVal> {
        match self {
            Val::Integer(val) => Some(*val as FloatVal),
            Val::Float(val) => Some(*val),
            _ => None,
        }
    }

    pub fn bytes(&self) -> Vec<u8> {
        match self {
            Val::Integer(val) => val.to_le_bytes().to_vec(),
            Val::Float(val) => val.to_le_bytes().to_vec(),
            Val::Compound(bytes, _) => bytes.clone(),
        }
    }
}

pub(super) struct Eval<'a, R: Read + Seek> {
    f: &'a mut R,
    scope: &'a Scope,
    symtab: &'a SymbolTable,
}

impl<'a, R: Read + Seek> Eval<'a, R> {
    pub fn new(
        f: &'a mut R,
        scope: &'a Scope,
        symtab: &'a SymbolTable,
    ) -> Self {
        Eval { f, scope, symtab }
    }

    pub fn eval(&mut self, expr: &'a Expr) -> EResult<Val> {
        match &expr.kind {
            ExprKind::Variable(_)
            | ExprKind::Member(_, _)
            | ExprKind::Index(_, _) => self.eval_access(expr).and_then(|n| {
                self.eval_name(n)
                    .ok_or_else(|| expr.err(EErrorKind::NonValue))
            }),
            ExprKind::Literal(kind) => match kind {
                LitKind::Int(i) => Ok(Val::Integer(*i)),
                LitKind::Char(i) => Ok(Val::Integer(*i as IntVal)),
                LitKind::Str(bytes) => Ok(Val::Compound(
                    bytes.clone(),
                    ByteSize(bytes.len() as u64).into(),
                )),
            },
            ExprKind::Attr(expr, attr) => self.eval_attribute(expr, *attr),
            ExprKind::Binary(op, lhs, rhs) => {
                self.eval_binary(*op, &lhs, &rhs)
            }
            ExprKind::Unary(op, expr) => self.eval_unary(*op, &expr),
            ExprKind::If(if_case) => self.eval_if(if_case),
        }
    }

    /* Evaluate until a name or value is found
     * For example:
     *      - a.b.c evaluates until c
     *      - a[0] evaluates until the array element
     *      - a remains a
     *      - 5 remains 5
     */
    pub fn eval_access(&mut self, expr: &'a Expr) -> EResult<Name> {
        Ok(match &expr.kind {
            ExprKind::Variable(sym) => {
                self.scope.get(*sym).ok_or_else(|| {
                    expr.err(EErrorKind::IdentifierNotFound(*sym))
                })?
            }
            ExprKind::Member(st, mem) => {
                if let FieldKind::Struct(ns) = self.expect_fk(&st)?.as_ref() {
                    Name::Field(
                        ns.fields
                            .sym_get(mem.sym)
                            .ok_or(EError(
                                mem.span,
                                EErrorKind::MemberNotFound(mem.sym),
                            ))?
                            .kind
                            .clone(),
                    )
                } else {
                    return Err(EError(
                        mem.span,
                        EErrorKind::NonStructMemberAccess,
                    ));
                }
            }
            ExprKind::Index(arr, idx_expr) => {
                let fk = self.expect_fk(arr)?;
                let idx = self.eval(&idx_expr)?;
                let i = idx.int().ok_or_else(|| {
                    idx_expr.err(EErrorKind::NonIntegerIndex)
                })?;
                let u = if i >= 0 {
                    i as usize
                } else {
                    return Err(idx_expr.err(EErrorKind::NegativeIndex));
                };
                if let FieldKind::Array(arr) = fk.as_ref() {
                    Name::Field(
                        arr.elements
                            .get(u)
                            .ok_or_else(|| {
                                expr.err(EErrorKind::ElementNotFound(u))
                            })?
                            .clone(),
                    )
                } else {
                    return Err(idx_expr.err(EErrorKind::NonArrayIndexAccess));
                }
            }
            _ => Name::Value(Rc::new(self.eval(expr)?)),
        })
    }

    pub fn eval_nonzero(&mut self, expr: &'a Expr) -> EResult<bool> {
        match self.eval(expr)? {
            Val::Integer(i) => Ok(i != 0),
            Val::Float(f) => Ok(f != 0.0),
            Val::Compound(data, _) => {
                for d in data {
                    if d != 0 {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
        }
    }

    fn eval_name(&mut self, name: Name) -> Option<Val> {
        Some(match name {
            Name::Value(val) => val.as_ref().clone(),
            Name::Field(fk) => match fk.as_ref() {
                FieldKind::Prim(ptr) => ptr.eval(self.f),
                FieldKind::Array(Array { start, size, .. })
                | FieldKind::Struct(Struct { start, size, .. }) => {
                    Val::Compound(
                        format::read_bytes(
                            *start,
                            *size,
                            Order::LittleEndian,
                            Order::LittleEndian,
                            self.f,
                        ),
                        *size,
                    )
                }
                FieldKind::Null(_) => return None,
            },
            _ => return None,
        })
    }

    fn expect_fk(&mut self, expr: &'a Expr) -> EResult<Rc<FieldKind>> {
        self.eval_access(expr)?
            .field()
            .ok_or_else(|| expr.err(EErrorKind::NonField))
    }

    fn eval_attribute(
        &mut self,
        expr: &'a Expr,
        attr: SpannedSym,
    ) -> EResult<Val> {
        let bi = self
            .symtab
            .attribute(attr.sym)
            .ok_or(EError(attr.span, EErrorKind::NotAnAttribute(attr.sym)))?;
        match bi {
            BuiltInAttr::Start => self.attr_start(expr),
            BuiltInAttr::Size => self.attr_size(expr),
            BuiltInAttr::End => self.attr_end(expr),
            BuiltInAttr::Offset => self.attr_offset(expr),
            BuiltInAttr::Length => self.attr_length(expr),
        }
    }

    fn attr_start(&mut self, expr: &'a Expr) -> EResult<Val> {
        let start = self.expect_fk(expr)?.start();
        Ok(Val::Integer(BytePos::from(start).size() as IntVal))
    }

    fn attr_size(&mut self, expr: &'a Expr) -> EResult<Val> {
        let start = self.expect_fk(expr)?.start();
        let size = self.expect_fk(expr)?.size();
        Ok(Val::Integer(
            ByteSize::from_unaligned(start, size).size() as IntVal
        ))
    }

    fn attr_end(&mut self, expr: &'a Expr) -> EResult<Val> {
        let start = self.expect_fk(expr)?.start();
        let size = self.expect_fk(expr)?.size();
        Ok(Val::Integer(BytePos::from(start + size).size() as IntVal))
    }

    fn attr_offset(&mut self, expr: &'a Expr) -> EResult<Val> {
        let offset = self.expect_fk(expr)?.start() - self.scope.base();
        Ok(Val::Integer(BytePos::from(offset).size() as IntVal))
    }

    fn attr_length(&mut self, expr: &'a Expr) -> EResult<Val> {
        if let FieldKind::Array(arr) = self.expect_fk(expr)?.as_ref() {
            Ok(Val::Integer(arr.elements.len() as IntVal))
        } else {
            Err(expr.err(EErrorKind::NonArray))
        }
    }

    pub fn eval_binary(
        &mut self,
        op: BinOp,
        lhs: &'a Expr,
        rhs: &'a Expr,
    ) -> EResult<Val> {
        let left = self.eval(&lhs)?;
        let right = self.eval(&rhs)?;

        match (&left, &right) {
            (Val::Integer(l), Val::Integer(r)) => {
                Some(Val::Integer(eval_binary_int(op, *l, *r)))
            }
            (Val::Float(l), Val::Integer(_)) => {
                eval_binary_float(op, *l, right.float().unwrap())
            }
            (Val::Integer(_), Val::Float(r)) => {
                eval_binary_float(op, left.float().unwrap(), *r)
            }
            (Val::Float(l), Val::Float(r)) => eval_binary_float(op, *l, *r),
            (Val::Compound(l, _), _) => {
                eval_binary_bytes(op, &l, &right.bytes())
            }
            (_, Val::Compound(r, _)) => {
                eval_binary_bytes(op, &left.bytes(), &r)
            }
        }
        .ok_or_else(|| {
            EError(lhs.span.merge(rhs.span), EErrorKind::BinaryTypeError)
        })
    }

    fn eval_unary(&mut self, op: UnOp, lhs: &'a Expr) -> EResult<Val> {
        let val = self.eval(&lhs)?;

        match val {
            Val::Integer(i) => Ok(Val::Integer(match op {
                UnOp::Neg => -i,
                UnOp::Not => (i == 0) as IntVal,
            })),
            Val::Float(f) => Ok(match op {
                UnOp::Neg => Val::Float(-f),
                UnOp::Not => Val::Integer((f == 0.0) as IntVal),
            }),
            Val::Compound(bytes, _) => match op {
                UnOp::Not => {
                    Ok(Val::Integer(bytes.iter().all(|b| *b == 0) as IntVal))
                }
                _ => Err(lhs.err(EErrorKind::UnaryCompound)),
            },
        }
    }

    fn eval_if(
        &mut self,
        if_case: &'a ast::IfCase<ast::Expr>,
    ) -> EResult<Val> {
        let cond = self.eval_nonzero(&if_case.cond)?;
        if cond {
            self.eval(&if_case.if_res)
        } else {
            self.eval(&if_case.else_res)
        }
    }
}

fn eval_binary_int(op: BinOp, lhs: IntVal, rhs: IntVal) -> IntVal {
    match op {
        BinOp::Add => lhs + rhs,
        BinOp::Sub => lhs - rhs,
        BinOp::Mul => lhs * rhs,
        BinOp::Div => lhs / rhs,
        BinOp::Rem => lhs % rhs,
        BinOp::BitAnd => lhs & rhs,
        BinOp::BitXor => lhs ^ rhs,
        BinOp::BitOr => lhs | rhs,
        BinOp::Shl => lhs << rhs,
        BinOp::Shr => lhs >> rhs,
        BinOp::Eq => (lhs == rhs) as IntVal,
        BinOp::Neq => (lhs != rhs) as IntVal,
        BinOp::And => (lhs != 0 && rhs != 0) as IntVal,
        BinOp::Or => (lhs != 0 || rhs != 0) as IntVal,
        BinOp::Lt => (lhs < rhs) as IntVal,
        BinOp::Gt => (lhs > rhs) as IntVal,
        BinOp::Leq => (lhs <= rhs) as IntVal,
        BinOp::Geq => (lhs >= rhs) as IntVal,
    }
}

fn eval_binary_float(op: BinOp, lhs: FloatVal, rhs: FloatVal) -> Option<Val> {
    let val = match op {
        BinOp::Add => Val::Float(lhs + rhs),
        BinOp::Sub => Val::Float(lhs - rhs),
        BinOp::Mul => Val::Float(lhs * rhs),
        BinOp::Div => Val::Float(lhs / rhs),
        BinOp::Rem => Val::Float(lhs % rhs),
        BinOp::Eq => {
            Val::Integer(((lhs - rhs).abs() < f64::EPSILON) as IntVal)
        }
        BinOp::Neq => {
            Val::Integer(((lhs - rhs).abs() > f64::EPSILON) as IntVal)
        }
        BinOp::And => Val::Integer((lhs != 0.0 && rhs != 0.0) as IntVal),
        BinOp::Or => Val::Integer((lhs != 0.0 || rhs != 0.0) as IntVal),
        BinOp::Lt => Val::Integer((lhs < rhs) as IntVal),
        BinOp::Gt => Val::Integer((lhs > rhs) as IntVal),
        BinOp::Leq => Val::Integer((lhs <= rhs) as IntVal),
        BinOp::Geq => Val::Integer((lhs >= rhs) as IntVal),
        _ => return None,
    };

    Some(val)
}

fn eval_binary_bytes(op: BinOp, lhs: &[u8], rhs: &[u8]) -> Option<Val> {
    match op {
        BinOp::Eq => Some(Val::Integer((lhs == rhs) as IntVal)),
        BinOp::Neq => Some(Val::Integer((lhs != rhs) as IntVal)),
        _ => None,
    }
}
