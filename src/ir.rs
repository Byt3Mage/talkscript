use crate::type_registry::TypeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
}

pub struct IrModule {
    pub functions: Vec<IrFunction>,
    pub globals: Vec<IrGlobal>,
}

pub struct IrFunction {
    pub name: String,
    pub params: Vec<IrParam>,
    pub ret_type: TypeId,
    pub body: IrBlock,
}

pub struct IrParam {
    pub name: String,
    pub ty: TypeId,
}

pub struct IrGlobal {
    pub name: String,
    pub ty: TypeId,
    pub init: Option<IrExpr>,
}

// Basic blocks for control flow
pub struct IrBlock {
    pub stmts: Vec<IrStmt>,
    pub terminator: IrTerminator,
}

pub enum IrStmt {
    // Variable binding
    Let {
        name: String,
        ty: TypeId,
        value: IrExpr,
    },
    // Assignment
    Assign {
        target: IrPlace,
        value: IrExpr,
    },
    // Expression statement
    Expr(IrExpr),
}

pub enum IrTerminator {
    // Return from function
    Return(IrExpr),

    // Unconditional jump
    Jump(BlockId),

    // Conditional branch
    Branch {
        cond: IrExpr,
        then_block: BlockId,
        else_block: BlockId,
    },

    // Loop structure
    Loop {
        body: BlockId,
        exit: BlockId,
    },

    // Break out of loop
    Break {
        target: BlockId,       // Which loop's exit
        value: Option<IrExpr>, // break value (for break expressions)
    },

    // Continue to next iteration
    Continue {
        target: BlockId, // Which loop's start
    },

    // Switch/match (for enums)
    Switch {
        value: IrExpr,
        cases: Vec<(usize, BlockId)>, // (discriminant, block)
        default: BlockId,
    },

    // Function doesn't return (diverges)
    Unreachable,
}

/// An expression that produces a value
pub enum IrExpr {
    // Literals
    Int(i64, TypeId),
    Float(f64, TypeId),
    Bool(bool, TypeId),
    String(String, TypeId), // String data + type (cstr or @str)
    Null(TypeId),           // null with its optional type

    // Use SSA variable
    Use(String, TypeId),

    // Phi node
    Phi {
        incoming: Vec<(BlockId, String)>,
        ty: TypeId,
    },

    // Operations
    Unary {
        op: UnaryOp,
        operand: Box<IrExpr>,
        ty: TypeId,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<IrExpr>,
        rhs: Box<IrExpr>,
        ty: TypeId,
    },

    // Function call
    Call {
        callee: Box<IrExpr>,
        args: Vec<IrExpr>,
        ty: TypeId, // Return type
    },

    // Struct/array operations
    StructLit {
        struct_ty: TypeId,
        fields: Vec<(String, IrExpr)>, // field name -> value
    },
    ArrayLit {
        element_ty: TypeId,
        elements: Vec<IrExpr>,
    },

    // Memory operations
    Ref {
        place: IrPlace,
        mutable: bool,
        ty: TypeId, // @T or @mut T
    },
    Deref {
        pointer: Box<IrExpr>,
        ty: TypeId, // T (from @T)
    },
}

/// A place (lvalue) that can be assigned to
pub enum IrPlace {
    // Local variable or parameter
    Local {
        name: String,
        ty: TypeId,
    },

    // Struct field access
    Field {
        base: Box<IrPlace>,
        field: String,
        ty: TypeId,
    },

    // Array/slice index
    Index {
        base: Box<IrPlace>,
        index: Box<IrExpr>,
        ty: TypeId,
    },

    // Pointer dereference (makes deref assignable)
    Deref {
        pointer: Box<IrExpr>,
        ty: TypeId,
    },
}
