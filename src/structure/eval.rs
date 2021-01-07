use std::io::{Read, Seek};

use super::error::{SErrorKind, SResult};
use super::scope::{Name, NameArray, NameFunc, NameStruct, Scope};
use super::*;
use crate::spec::ast::{BinOp, Expr, ExprKind, UnOp};
use crate::spec::LitKind;

pub fn eval<'a, R: Read + Seek>(
    expr: &'a Expr,
    f: &mut R,
    scope: &Scope,
) -> SResult<Val> {
    Eval::new(f, scope).eval(expr)
}

pub fn eval_partial<'a, R: Read + Seek>(
    expr: &'a Expr,
    f: &'a mut R,
    scope: &'a Scope<'a>,
) -> SResult<Partial<'a>> {
    Eval::new(f, scope).eval_partial(expr)
}

pub type IntVal = i64;
pub type FloatVal = f64;

#[derive(Clone, Debug)]
pub enum Val {
    Integer(IntVal),
    Float(FloatVal),
    Compound(Vec<u8>, u64),
}

pub enum Partial<'a> {
    Value(Val),
    Name(&'a Name<'a>),
}

impl<'a> Partial<'a> {
    pub fn name(self) -> SResult<&'a Name<'a>> {
        if let Partial::Name(name) = self {
            Ok(name)
        } else {
            Err(SErrorKind::InvalidType)
        }
    }
}

impl Val {
    pub fn int(&self) -> SResult<IntVal> {
        if let Val::Integer(val) = self {
            Ok(*val)
        } else {
            Err(SErrorKind::NotAnIntegerValue)
        }
    }

    pub fn float(&self) -> SResult<FloatVal> {
        match self {
            Val::Integer(val) => Ok(*val as FloatVal),
            Val::Float(val) => Ok(*val),
            _ => Err(SErrorKind::NotAFloatValue),
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

    fn eval(&mut self, expr: &'a Expr) -> SResult<Val> {
        Ok(match &expr.kind {
            ExprKind::Variable(_)
            | ExprKind::Member(_, _)
            | ExprKind::Index(_, _) => {
                let name = self.eval_partial(expr)?.name()?;
                self.eval_name(name)?
            }
            ExprKind::Literal(kind) => match kind {
                LitKind::Int(i) => Val::Integer(*i),
                LitKind::Char(i) => Val::Integer(*i as IntVal),
                LitKind::Str(bytes) => {
                    Val::Compound(bytes.clone(), (bytes.len() * 8) as u64)
                }
            },
            //ExprKind::Array(elems) => todo!(),
            ExprKind::Call(func, args) => self.eval_call(func, args)?,
            ExprKind::AddrOf(obj) => self.eval_addrof(obj)?,
            ExprKind::Binary(op, lhs, rhs) => {
                self.eval_binary(*op, &lhs, &rhs)?
            }
            ExprKind::Unary(op, expr) => self.eval_unary(*op, &expr)?,
        })
    }

    fn eval_partial(&mut self, expr: &'a Expr) -> SResult<Partial<'a>> {
        Ok(match &expr.kind {
            ExprKind::Variable(sym) => Partial::Name(self.scope.get(*sym)?),
            ExprKind::Member(st, sym) => {
                let struct_name = self.eval_partial(&st)?.name()?;
                Partial::Name(struct_name.get_field(*sym)?)
            }
            ExprKind::Index(arr, idx_expr) => {
                let arr_name = self.eval_partial(&arr)?.name()?;
                let idx = self.eval(&idx_expr)?;
                Partial::Name(arr_name.get_element(idx.int()? as usize)?)
            }
            _ => Partial::Value(self.eval(expr)?),
        })
    }

    fn eval_name(&mut self, name: &'a Name<'a>) -> SResult<Val> {
        Ok(match name {
            Name::Value(val) => val.clone(),
            Name::Field(ptr) => ptr.eval(self.f),
            Name::Array(NameArray { start, size, .. })
            | Name::Struct(NameStruct { start, size, .. }) => Val::Compound(
                format::read_bytes(*start, *size, Order::LittleEndian, self.f),
                *size,
            ),
            Name::Reference(_) => unreachable!(),
            _ => return Err(SErrorKind::InvalidType),
        })
    }

    fn eval_call(&mut self, func: &'a Expr, args: &'a [Expr]) -> SResult<Val> {
        if let Name::Func(nfunc) = self.eval_partial(func)?.name()? {
            match (nfunc, args) {
                (NameFunc::AddrOf, [lval]) => self.eval_addrof(lval),
                (NameFunc::SizeOf, [lval]) => self.eval_sizeof(lval),
                (NameFunc::EndOf, [lval]) => self.eval_endof(lval),
                (NameFunc::Len, [lval]) => self.eval_len(lval),
                _ => Err(SErrorKind::InvalidType),
            }
        } else {
            Err(SErrorKind::InvalidType)
        }
    }

    fn eval_addrof(&mut self, expr: &'a Expr) -> SResult<Val> {
        self.eval_partial(expr)?
            .name()?
            .start()
            .map(|a| Val::Integer(a as IntVal / 8))
            .ok_or(SErrorKind::InvalidType)
    }

    fn eval_sizeof(&mut self, expr: &'a Expr) -> SResult<Val> {
        self.eval_partial(expr)?
            .name()?
            .size()
            .map(|a| Val::Integer(a as IntVal / 8))
            .ok_or(SErrorKind::InvalidType)
    }

    fn eval_endof(&mut self, expr: &'a Expr) -> SResult<Val> {
        let start = self
            .eval_partial(expr)?
            .name()?
            .start()
            .ok_or(SErrorKind::InvalidType)?;
        let size = self
            .eval_partial(expr)?
            .name()?
            .size()
            .ok_or(SErrorKind::InvalidType)?;
        Ok(Val::Integer((start - size) as IntVal / 8))
    }

    fn eval_len(&mut self, expr: &'a Expr) -> SResult<Val> {
        Ok(Val::Integer(match self.eval_partial(expr)?.name()? {
            Name::Struct(nst) => nst.fields.len(),
            Name::Array(narr) => narr.elements.len(),
            _ => return Err(SErrorKind::InvalidType),
        } as IntVal))
    }

    fn eval_binary(
        &mut self,
        op: BinOp,
        lhs: &'a Expr,
        rhs: &'a Expr,
    ) -> SResult<Val> {
        let left = self.eval(&lhs)?;
        let right = self.eval(&rhs)?;

        match (&left, &right) {
            (Val::Integer(l), Val::Integer(r)) => {
                Ok(Val::Integer(self.eval_binary_int(op, *l, *r)))
            }
            (Val::Float(l), Val::Integer(_)) => {
                self.eval_binary_float(op, *l, right.float().unwrap())
            }
            (Val::Integer(_), Val::Float(r)) => {
                self.eval_binary_float(op, left.float().unwrap(), *r)
            }
            (Val::Float(l), Val::Float(r)) => {
                self.eval_binary_float(op, *l, *r)
            }
            (Val::Compound(l, _), _) => {
                self.eval_binary_bytes(op, &l, &right.bytes())
            }
            (_, Val::Compound(r, _)) => {
                self.eval_binary_bytes(op, &left.bytes(), &r)
            }
        }
    }

    fn eval_binary_int(
        &mut self,
        op: BinOp,
        lhs: IntVal,
        rhs: IntVal,
    ) -> IntVal {
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

    fn eval_binary_float(
        &mut self,
        op: BinOp,
        lhs: FloatVal,
        rhs: FloatVal,
    ) -> SResult<Val> {
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
            _ => return Err(SErrorKind::InvalidType),
        };

        Ok(val)
    }

    fn eval_binary_bytes(
        &mut self,
        op: BinOp,
        lhs: &[u8],
        rhs: &[u8],
    ) -> SResult<Val> {
        match op {
            BinOp::Eq => Ok(Val::Integer((lhs == rhs) as IntVal)),
            BinOp::Neq => Ok(Val::Integer((lhs != rhs) as IntVal)),
            _ => Err(SErrorKind::InvalidType),
        }
    }

    fn eval_unary(&mut self, op: UnOp, lhs: &'a Expr) -> SResult<Val> {
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
                _ => Err(SErrorKind::InvalidType),
            },
        }
    }
}
