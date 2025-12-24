use super::tokens::Span;
use crate::arena::{Arena, Ident, StrSymbol};

slotmap::new_key_type! {
    pub struct ExprId;
    pub struct StmtId;
    pub struct DeclId;
    pub struct PatternId;
}

pub struct CanonAstArena {
    pub exprs: Arena<ExprId, Expr>,
    pub stmts: Arena<StmtId, Stmt>,
    pub decls: Arena<DeclId, Decl>,
    pub patterns: Arena<PatternId, Pattern>,
}

impl CanonAstArena {
    pub fn new() -> Self {
        Self {
            exprs: Arena::with_key(),
            stmts: Arena::with_key(),
            decls: Arena::with_key(),
            patterns: Arena::with_key(),
        }
    }
}

// ============================================================================
// Expressions
// ============================================================================

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    // Literals
    Int(i64),
    Uint(u64),
    Float(f64),
    Bool(bool),
    Char(char),
    CStr(StrSymbol),
    Null,
    Void,

    // Identifiers
    Ident(Ident),

    // Grouping
    Group(ExprId),

    // Array Literals
    ArrayLit(Vec<ExprId>),

    ArrayRep {
        value: ExprId,
        count: ExprId,
    },

    // Struct/Union Literals
    StructLit {
        ty: Option<ExprId>,
        fields: Vec<FieldInit>,
    },

    // Enum Literals
    EnumLit {
        ty: Option<ExprId>,
        variant: Ident,
    },

    // Tuple Literals
    TupleLit {
        fields: Vec<ExprId>,
    },

    // Operators
    Unary {
        op: UnaryOp,
        expr: ExprId,
    },
    Binary {
        op: BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },

    // Assignment
    Assign {
        op: AssignOp,
        tgt: ExprId,
        val: ExprId,
    },

    // Type Operations
    Cast {
        expr: ExprId,
        ty: ExprId,
    },

    // Control Flow
    If {
        cond: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    },
    Match {
        expr: ExprId,
        arms: Vec<MatchArm>,
    },
    While {
        cond: ExprId,
        body: ExprId,
    },
    Loop(ExprId),
    For {
        pattern: PatternId,
        iter: ExprId,
        body: ExprId,
    },

    // Blocks
    Block(Vec<StmtId>),

    // Jump Statements
    Return(ExprId),
    Break(Option<ExprId>),
    Continue,

    // Function Call
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },

    // Field Access
    Field {
        object: ExprId,
        field: Ident,
    },

    // Scope Access
    ScopeAccess {
        object: ExprId,
        name: Ident,
    },

    // Optional Field Access
    OptionalField {
        object: ExprId,
        field: Ident,
    },

    // Index
    Index {
        obj: ExprId,
        idx: ExprId,
    },

    // Range
    Range {
        start: Option<ExprId>,
        end: Option<ExprId>,
        inclusive: bool,
    },

    // Optional Operators
    Unwrap(ExprId),

    // Comptime Expression
    Comptime(ExprId),

    // Type Literals
    ModuleType {
        decls: Vec<DeclId>,
    },

    StructType {
        fields: Vec<Field>,
        decls: Vec<DeclId>,
    },

    UnionType {
        fields: Vec<Field>,
        decls: Vec<DeclId>,
    },

    EnumType {
        variants: Vec<EnumVariant>,
        decls: Vec<DeclId>,
    },

    TupleType {
        fields: Vec<ExprId>,
    },

    ArrayType {
        elem_ty: ExprId,
        len: ExprId,
    },

    PointerType {
        pointee: ExprId,
        mutable: bool,
    },

    OptionalType(ExprId),

    FnType {
        params: Vec<(Option<Ident>, ExprId)>,
        ret: ExprId,
    },
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub name: Ident,
    pub value: Option<ExprId>,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Ref,
    RefMut,
    Deref,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    UnwrapOr,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: PatternId,
    pub guard: Option<ExprId>,
    pub body: ExprId,
}

// ============================================================================
// Statements
// ============================================================================

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Semi,

    VarDecl {
        is_comptime: bool,
        mutable: bool,
        pattern: PatternId,
        ty: Option<ExprId>,
        value: ExprId,
    },

    Expr {
        expr: ExprId,
        has_semi: bool,
    },
}

// ============================================================================
// Patterns
// ============================================================================

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Wildcard,

    Variable {
        mutable: bool,
        name: Ident,
    },

    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(StrSymbol),

    Struct {
        path: Option<ExprId>,
        fields: Vec<NamedFieldPattern>,
    },

    Tuple {
        elements: Vec<PatternId>,
    },

    Enum {
        path: Option<ExprId>,
        variant: Ident,
    },

    Lit(ExprId),

    Or(Vec<PatternId>),

    Range {
        start: Option<ExprId>,
        end: Option<ExprId>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone)]
pub struct NamedFieldPattern {
    pub name: Ident,
    pub pattern: PatternId,
    pub span: Span,
}

// ============================================================================
// Declarations (lowered/canonical form)
// ============================================================================

#[derive(Debug, Clone)]
pub struct Decl {
    pub visibility: Visibility,
    pub name: Ident,
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub enum DeclKind {
    Const {
        ty: Option<ExprId>,
        value: ExprId,
    },

    Function {
        is_comptime: bool,
        params: Vec<Param>,
        ret: ExprId,
        body: ExprId,
    },
}

// ============================================================================
// Comptime Parameters
// ============================================================================

#[derive(Clone, Debug, Default)]
pub struct ComptimeParams {
    pub params: Vec<ComptimeParam>,
}

impl ComptimeParams {
    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct ComptimeParam {
    pub name: Ident,
    pub ty: ExprId,
    pub span: Span,
}

// ============================================================================
// Fields and Variants
// ============================================================================

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: ExprId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Ident,
    pub value: Option<ExprId>,
    pub span: Span,
}

// ============================================================================
// Function Parameters
// ============================================================================

#[derive(Debug, Clone)]
pub struct Param {
    pub is_comptime: bool,
    pub mutable: bool,
    pub name: Ident,
    pub ty: ExprId,
    pub span: Span,
}

// ============================================================================
// Helpers
// ============================================================================

impl Expr {
    pub fn is_assignable(&self) -> bool {
        matches!(
            self.kind,
            ExprKind::Ident(_) | ExprKind::Field { .. } | ExprKind::Index { .. }
        )
    }
}
