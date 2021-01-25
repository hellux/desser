use std::io::{Read, Seek};

use super::error::{EError, EErrorKind, EResult};
use super::scope::{Name, NameArray, NameField, NameFunc, NameStruct, Scope};
use super::*;
use crate::spec::ast::{BinOp, Expr, ExprKind, UnOp};
use crate::spec::LitKind;

pub(super) fn eval<'a, R: Read + Seek>(
    expr: &'a Expr,
    f: &mut R,
    scope: &Scope,
) -> EResult<Val> {
    Eval::new(f, scope).eval(expr)
}

pub(super) fn eval_partial<'a, R: Read + Seek>(
    expr: &'a Expr,
    f: &'a mut R,
    scope: &'a Scope<'a>,
) -> EResult<Partial<'a>> {
    Eval::new(f, scope).eval_partial(expr)
}

pub type IntVal = i64;
pub type FloatVal = f64;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Integer(IntVal),
    Float(FloatVal),
    Compound(Vec<u8>, BitSize),
}

#[derive(Clone, Debug)]
pub(super) enum Partial<'a> {
    Value(Val),
    Name(Name<'a>),
}

impl<'a> Partial<'a> {
    pub fn name(self) -> Option<Name<'a>> {
        if let Partial::Name(name) = self {
            Some(name)
        } else {
            None
        }
    }
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

struct Eval<'a, R: Read + Seek> {
    f: &'a mut R,
    scope: &'a Scope<'a>,
}

impl<'a, R: Read + Seek> Eval<'a, R> {
    fn new(f: &'a mut R, scope: &'a Scope<'a>) -> Self {
        Eval { f, scope }
    }

    fn eval(&mut self, expr: &'a Expr) -> EResult<Val> {
        match &expr.kind {
            ExprKind::Variable(_)
            | ExprKind::Member(_, _)
            | ExprKind::Index(_, _) => self.eval_partial(expr).and_then(|p| {
                self.eval_name(p.name().unwrap())
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
            ExprKind::Call(func, args) => self.eval_call(func, args),
            ExprKind::AddrOf(obj) => self.eval_addrof(obj),
            ExprKind::Binary(op, lhs, rhs) => {
                self.eval_binary(*op, &lhs, &rhs)
            }
            ExprKind::Unary(op, expr) => self.eval_unary(*op, &expr),
        }
    }

    fn eval_partial(&mut self, expr: &'a Expr) -> EResult<Partial<'a>> {
        Ok(match &expr.kind {
            ExprKind::Variable(sym) => {
                Partial::Name(self.scope.get(*sym).ok_or_else(|| {
                    expr.err(EErrorKind::IdentifierNotFound(*sym))
                })?)
            }
            ExprKind::Member(st, sym) => {
                let struct_name = self
                    .eval_partial(&st)?
                    .name()
                    .and_then(|n| n.field())
                    .ok_or_else(|| st.err(EErrorKind::NonStruct))?;
                Partial::Name(Name::Field(
                    struct_name.get_field(*sym).ok_or_else(|| {
                        expr.err(EErrorKind::MemberNotFound(*sym))
                    })?,
                ))
            }
            ExprKind::Index(arr, idx_expr) => {
                let arr_name = self
                    .eval_partial(&arr)?
                    .name()
                    .and_then(|n| n.field())
                    .ok_or_else(|| arr.err(EErrorKind::NonArray))?;
                let idx = self.eval(&idx_expr)?;
                let i = idx.int().ok_or_else(|| {
                    idx_expr.err(EErrorKind::NonIntegerIndex)
                })?;
                let u = if i >= 0 {
                    i as usize
                } else {
                    return Err(idx_expr.err(EErrorKind::NegativeIndex));
                };
                Partial::Name(Name::Field(
                    arr_name.get_element(u).ok_or_else(|| {
                        expr.err(EErrorKind::ElementNotFound(u))
                    })?,
                ))
            }
            _ => Partial::Value(self.eval(expr)?),
        })
    }

    fn eval_name(&mut self, name: Name<'a>) -> Option<Val> {
        Some(match name {
            Name::Value(val) => val.clone(),
            Name::Field(nf) => match nf {
                NameField::Prim(ptr) => ptr.eval(self.f),
                NameField::Array(NameArray { start, size, .. })
                | NameField::Struct(NameStruct { start, size, .. }) => {
                    Val::Compound(
                        format::read_bytes(
                            *start,
                            *size,
                            Order::LittleEndian,
                            self.f,
                        ),
                        *size,
                    )
                }
            },
            _ => return None,
        })
    }

    fn eval_call(&mut self, func: &'a Expr, args: &'a [Expr]) -> EResult<Val> {
        if let Name::Func(nfunc) = self
            .eval_partial(func)?
            .name()
            .ok_or_else(|| func.err(EErrorKind::NonFunction))?
        {
            match (nfunc, args) {
                (NameFunc::AddrOf, [lval]) => self.eval_addrof(lval),
                (NameFunc::SizeOf, [lval]) => self.eval_sizeof(lval),
                (NameFunc::EndOf, [lval]) => self.eval_endof(lval),
                (NameFunc::OffsOf, [lval]) => self.eval_offsof(lval),
                (NameFunc::Len, [lval]) => self.eval_len(lval),
                _ => Err(func.err(EErrorKind::ArgumentMismatch)),
            }
        } else {
            Err(func.err(EErrorKind::NonFunction))
        }
    }

    fn eval_addrof(&mut self, expr: &'a Expr) -> EResult<Val> {
        self.eval_partial(expr)?
            .name()
            .and_then(|n| n.field())
            .ok_or_else(|| expr.err(EErrorKind::NonField))
            .map(|nf| Val::Integer(BytePos::from(nf.start()).size() as IntVal))
    }

    fn eval_sizeof(&mut self, expr: &'a Expr) -> EResult<Val> {
        let start = self
            .eval_partial(expr)?
            .name()
            .and_then(|n| n.field())
            .ok_or_else(|| expr.err(EErrorKind::NonField))
            .map(NameField::start)?;
        let size = self
            .eval_partial(expr)?
            .name()
            .and_then(|n| n.field())
            .ok_or_else(|| expr.err(EErrorKind::NonField))
            .map(NameField::size)?;
        Ok(Val::Integer(
            ByteSize::from_unaligned(start, size).size() as IntVal
        ))
    }

    fn eval_endof(&mut self, expr: &'a Expr) -> EResult<Val> {
        let start = self
            .eval_partial(expr)?
            .name()
            .and_then(|n| n.field())
            .ok_or_else(|| expr.err(EErrorKind::NonField))
            .map(NameField::start)?;
        let size = self
            .eval_partial(expr)?
            .name()
            .and_then(|n| n.field())
            .ok_or_else(|| expr.err(EErrorKind::NonField))
            .map(NameField::start)?;
        Ok(Val::Integer(BytePos::from(start + size).size() as IntVal))
    }

    fn eval_offsof(&mut self, expr: &'a Expr) -> EResult<Val> {
        self.eval_partial(expr)?
            .name()
            .and_then(|n| n.field())
            .ok_or_else(|| expr.err(EErrorKind::NonField))
            .map(|nf| {
                Val::Integer(
                    (BytePos::from(nf.start() - self.scope.base())).size()
                        as IntVal,
                )
            })
    }

    fn eval_len(&mut self, expr: &'a Expr) -> EResult<Val> {
        if let Some(NameField::Array(narr)) =
            self.eval_partial(expr)?.name().and_then(|n| n.field())
        {
            Ok(Val::Integer(narr.elements.len() as IntVal))
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
