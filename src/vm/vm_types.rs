pub type StrLit = usize;
pub type Integer = i64;
pub type Float = f64;
pub type Boolean = bool;
pub type Pointer = usize;
pub type RegId = usize;
pub type LabelId = usize;

#[derive(Debug, Copy, Clone)]
pub struct FunctionInfo {
    /// Entry point in the list of instructions.
    pub entry_pc: usize,
    /// Number of arguments this function has.
    pub arity: u8,
    /// Number of registers allocated for this function.
    pub num_regs: u8,
}
