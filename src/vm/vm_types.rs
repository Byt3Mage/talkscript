use crate::vm::{Value, vm_types::private::Sealed};

pub type StrLit = u64;
pub type Integer = i64;
pub type Float = f64;
pub type Boolean = bool;
pub type RegId = usize;
pub type LabelId = usize;

/// [gen(1) | index(62)]
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ptr(u64);

impl Ptr {
    const GEN_BIT: u64 = 1u64 << 63;

    #[inline]
    pub const fn nursery(pos: u64) -> Self {
        Self(pos)
    }

    #[inline]
    pub const fn old(pos: u64) -> Self {
        Self(pos | Self::GEN_BIT)
    }

    #[inline]
    pub const fn is_old(self) -> bool {
        (self.0 & Self::GEN_BIT) != 0
    }

    #[inline]
    pub const fn position(self) -> u64 {
        self.0 & !Self::GEN_BIT
    }

    #[inline]
    pub const fn raw(self) -> u64 {
        self.0
    }

    #[inline]
    pub const fn from_raw(val: u64) -> Self {
        Self(val)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunctionInfo {
    /// Entry point in the list of instructions.
    pub entry_pc: usize,
    /// Number of arguments this function has.
    pub arity: u8,
    /// Number of registers allocated for this function.
    pub num_regs: u8,
}

mod private {
    pub trait Sealed {}
}

impl Sealed for Integer {}
impl Sealed for Float {}
impl Sealed for Boolean {}
impl Sealed for Ptr {}

pub trait VMType: Sealed {
    fn to_value(self) -> Value;
}

impl VMType for Integer {
    #[inline(always)]
    fn to_value(self) -> Value {
        self as Value
    }
}

impl VMType for Float {
    #[inline(always)]
    fn to_value(self) -> Value {
        Float::to_bits(self)
    }
}

impl VMType for Boolean {
    #[inline(always)]
    fn to_value(self) -> Value {
        self as Value
    }
}

impl VMType for Ptr {
    fn to_value(self) -> Value {
        self.0 as Value
    }
}
