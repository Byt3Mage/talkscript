use std::{collections::HashMap, sync::Arc};

use crate::vm::{VMResult, instruction::Instruction, object::Value, safepoint::SafePointTable};

/// A single compilation unit
pub struct Unit {
    /// Bytecode for all functions in this unit
    pub bytecode: Arc<[Instruction]>,

    /// All constants used in this unit
    pub constants: Arc<[Value]>,

    /// All bytecode functions defined in this unit
    pub functions: Arc<[FuncInfo]>,

    /// All native functions defined in this unit
    pub native_functions: Arc<[NativeFuncInfo]>,

    /// External compilation units this unit depends on
    pub imports: Arc<[Arc<Unit>]>,

    /// Items this unit exposes to other units
    pub exports: ExportTable,
}

#[derive(Debug, Clone)]
pub struct CallInfo {
    /// Entry point in the list of instructions
    pub entry_pc: usize,

    /// Number of registers allocated for this function
    pub nreg: u8,

    /// Number of arguments this function expects
    pub narg: u8,

    /// Number of registers used for return value
    pub nret: u8,

    /// Start register for return value
    pub ret_reg: u8,

    /// Safe points containing map of pointer registers
    pub safe_points: SafePointTable,
}

#[derive(Debug, Clone)]
pub struct FuncInfo {
    /// Function name with full path for debugging
    pub name: String,

    /// Which module owns the actual bytecode?
    /// - None => this module,
    /// - Some(`id`) => imported module with local `id`
    pub unit_id: Option<usize>,

    /// Information needed by the call frame during execution
    pub call_info: CallInfo,
}

pub struct NativeFuncInfo {
    /// Function implementation
    pub(crate) func: Box<dyn Fn(&[Value], &mut [Value]) -> VMResult<()>>,

    /// Number of argument registers this function expects
    pub(crate) narg: u8,

    /// Number of return registers this function uses
    pub(crate) nret: u8,
}

pub struct ExportTable {
    /// Top-level exported functions
    pub functions: HashMap<String, FunctionExport>,

    /// Top-level exported native functions
    pub native_functions: HashMap<String, NativeFunctionExport>,

    /// Nested namespaces from mod blocks
    pub namespaces: HashMap<String, ExportTable>,
}

pub struct FunctionExport {
    /// Index into [Unit::functions]
    pub index: usize,
    // type-checking info
}

pub struct NativeFunctionExport {
    /// Index into [Unit::native_functions]
    pub index: usize,
    // type-checking info
}
