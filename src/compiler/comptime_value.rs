use std::{
    hash::Hash,
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

use crate::{
    arena::StrSymbol,
    compiler::{hir::HirDeclId, type_info::TypeId},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComptimeValue {
    Type(TypeId),
    Int(i64),
    Float(ComptimeFloat),
    Bool(bool),
    Cstr(StrSymbol),
    Char(char),
    Void,
    Array(Rc<[ComptimeValue]>),
    Option(Option<Box<ComptimeValue>>),
    Function { decl_id: HirDeclId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ComptimeFloat(u64);

impl ComptimeFloat {
    pub fn new(val: f64) -> Self {
        Self(f64::to_bits(val))
    }

    fn get(self) -> f64 {
        f64::from_bits(self.0)
    }
}

impl Neg for ComptimeFloat {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new(-self.get())
    }
}

impl Add for ComptimeFloat {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.get() + rhs.get())
    }
}

impl Sub for ComptimeFloat {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.get() - rhs.get())
    }
}

impl Mul for ComptimeFloat {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(self.get() - rhs.get())
    }
}

impl Div for ComptimeFloat {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(self.get() - rhs.get())
    }
}
