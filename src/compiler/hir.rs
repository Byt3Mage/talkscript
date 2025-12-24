// src/compiler/hir.rs

use crate::arena::{Arena, Ident};
use crate::compiler::tokens::Span;
use ahash::AHashMap;

slotmap::new_key_type! {
    pub struct HirInstId;
    pub struct HirBlockId;
    pub struct HirDeclId;
}

// ============================================================================
// Module - Top Level
// ============================================================================

pub struct HirModule {
    pub decls: Arena<HirDeclId, HirDecl>,
    pub decl_map: AHashMap<Ident, HirDeclId>,
}

pub enum HirDecl {
    Const {
        name: Ident,
        ty: Option<HirInstId>,
        value: HirInstId,
        span: Span,
    },
    Function(HirFunction),
}

// ============================================================================
// Functions
// ============================================================================

pub struct HirFunction {
    pub name: Ident,
    pub is_comptime: bool,
    pub params: Vec<HirParam>,
    pub ret_type: HirInstId,
    pub entry_block: HirBlockId,
    pub blocks: Arena<HirBlockId, HirBlock>,
    pub span: Span,
}

pub struct HirParam {
    pub is_comptime: bool,
    pub mutable: bool,
    pub name: Ident,
    pub ty: HirInstId,
    pub span: Span,
}

// ============================================================================
// Blocks
// ============================================================================

pub struct HirBlock {
    pub params: Vec<(Ident, HirInstId)>, // Block parameters (for phi)
    pub insts: Vec<HirInstId>,
    pub terminator: HirTerminator,
}

// ============================================================================
// Instructions
// ============================================================================

pub struct HirInst {
    pub kind: HirInstKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirInstKind {
    // ==================== Literals ====================
    Int(i64),
    Uint(u64),
    Float(f64),
    Bool(bool),
    Char(char),
    Cstr(Ident),
    Null,
    Void,

    // ==================== Variables ====================
    DeclVar {
        name: Ident,
        is_comptime: bool,
        mutable: bool,
    },
    Ref {
        name: Ident,
    },
    Load {
        var: HirInstId,
    },
    Store {
        var: HirInstId,
        value: HirInstId,
    },

    // ==================== Arithmetic ====================
    Add(HirInstId, HirInstId),
    Sub(HirInstId, HirInstId),
    Mul(HirInstId, HirInstId),
    Div(HirInstId, HirInstId),
    Mod(HirInstId, HirInstId),
    Neg(HirInstId),

    BitAnd(HirInstId, HirInstId),
    BitOr(HirInstId, HirInstId),
    BitXor(HirInstId, HirInstId),
    BitNot(HirInstId),
    Shl(HirInstId, HirInstId),
    Shr(HirInstId, HirInstId),

    // ==================== Comparisons ====================
    Eq(HirInstId, HirInstId),
    Ne(HirInstId, HirInstId),
    Lt(HirInstId, HirInstId),
    Gt(HirInstId, HirInstId),
    Le(HirInstId, HirInstId),
    Ge(HirInstId, HirInstId),

    // ==================== Logical ====================
    And(HirInstId, HirInstId),
    Or(HirInstId, HirInstId),
    Not(HirInstId),

    // ==================== Aggregates ====================
    ArrayLit(Vec<HirInstId>),
    ArrayRepeat {
        value: HirInstId,
        count: HirInstId,
    },
    StructLit {
        ty: Option<HirInstId>,
        fields: Vec<(Ident, HirInstId)>,
    },
    TupleLit(Vec<HirInstId>),
    EnumLit {
        ty: Option<HirInstId>,
        variant: Ident,
    },
    UnionLit {
        ty: Option<HirInstId>,
        field: Ident,
        value: HirInstId,
    },

    Index {
        object: HirInstId,
        index: HirInstId,
    },
    Field {
        object: HirInstId,
        field: Ident,
    },
    ScopeAccess {
        object: HirInstId,
        name: Ident,
    },

    // ==================== Optional ====================
    Some(HirInstId),
    IsSome(HirInstId),
    Unwrap(HirInstId),
    UnwrapOr {
        optional: HirInstId,
        default: HirInstId,
    },

    // ==================== Function Calls ====================
    Call {
        callee: HirInstId,
        args: Vec<HirInstId>,
    },

    // ==================== Type Operations ====================
    StructType {
        fields: Vec<(Ident, HirInstId)>,
        methods: Vec<HirDeclId>,
    },
    EnumType {
        variants: Vec<(Ident, Option<HirInstId>)>,
        methods: Vec<HirDeclId>,
    },
    UnionType {
        fields: Vec<(Ident, HirInstId)>,
        methods: Vec<HirDeclId>,
    },
    TupleType(Vec<HirInstId>),
    ArrayType {
        elem_ty: HirInstId,
        len: HirInstId,
    },
    OptionalType(HirInstId),
    PointerType {
        pointee: HirInstId,
        mutable: bool,
    },
    FunctionType {
        params: Vec<HirInstId>,
        ret: HirInstId,
    },
    ModuleType {
        decls: Vec<HirDeclId>,
    },

    // ==================== Block Phi ====================
    BlockParam {
        block: HirBlockId,
        index: usize,
    },
}

// ============================================================================
// Terminators
// ============================================================================

#[derive(Debug, Clone)]
pub enum HirTerminator {
    Return(HirInstId),

    Jump {
        target: HirBlockId,
        args: Vec<HirInstId>,
    },

    Branch {
        cond: HirInstId,
        then_target: HirBlockId,
        then_args: Vec<HirInstId>,
        else_target: HirBlockId,
        else_args: Vec<HirInstId>,
    },

    Switch {
        value: HirInstId,
        cases: Vec<(i64, HirBlockId, Vec<HirInstId>)>,
        default: HirBlockId,
        default_args: Vec<HirInstId>,
    },

    Unreachable,
}
