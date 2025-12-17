use super::tokens::Span;
use crate::arena::{Arena, Interner, StrSymbol};

pub type Ident = StrSymbol;

slotmap::new_key_type! {
    pub struct ExprId;
    pub struct StmtId;
    pub struct ItemId;
    pub struct TypeId;
    pub struct PathId;
    pub struct PatternId;
}

pub struct AstArena {
    pub exprs: Arena<ExprId, Expr>,
    pub stmts: Arena<StmtId, Stmt>,
    pub items: Arena<ItemId, Item>,
    pub types: Arena<TypeId, Type>,
    pub paths: Arena<PathId, Path>,
    pub patterns: Arena<PatternId, Pattern>,
}

impl AstArena {
    pub fn new() -> Self {
        Self {
            exprs: Arena::with_key(),
            stmts: Arena::with_key(),
            items: Arena::with_key(),
            types: Arena::with_key(),
            paths: Arena::with_key(),
            patterns: Arena::with_key(),
        }
    }
}

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    // Primitive types
    Any,
    Int,
    Float,
    Bool,
    String,
    Char,
    Void,
    Never,

    // Special comptime type (the type of types)
    Type,

    // Path (named type or generic parameter)
    Path(PathId),

    // Optional
    Optional(TypeId),

    // Array types
    Array {
        element: TypeId,
        len: ExprId, // Comptime-evaluable expression
    },

    DynArray(TypeId), // [T] - unsized

    // Pointer
    Pointer {
        pointee: TypeId,
        mutable: bool,
    },

    // Function type: fn(int, float) -> bool
    Function {
        params: Vec<TypeId>,
        ret: TypeId,
    },

    // Anonymous struct - always named fields
    // struct { x: int, y: float }
    AnonStruct {
        fields: Vec<Field>,
        methods: Vec<ItemId>,
    },

    // Anonymous tuple - always unnamed fields
    // tuple { int, float }
    Tuple {
        fields: Vec<TypeId>,
    },

    // Anonymous enum - variants map to types
    // enum { Ok: cstr, Err: int, None } // None => implicit void type
    AnonEnum {
        variants: Vec<Field>,
        methods: Vec<ItemId>,
    },

    // Range type
    Range {
        element: TypeId,
        inclusive: bool,
    },

    // Inferred type: _
    Infer,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: TypeId,
    pub span: Span,
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
    Float(f64),
    Bool(bool),
    Char(char),
    CStr(StrSymbol),
    Null,
    Void,

    // Type literal (for comptime type values)
    // const T: type = int;
    TypeLit(TypeId),

    // Identifiers and paths
    Ident(Ident),
    Path(PathId),

    // Grouping
    Group(ExprId),

    // Array literal
    // [1, 2, 3, 4, 5]
    ArrayLit(Vec<ExprId>),

    // Array repeat (value repeated N times)
    ArrayRepeat {
        value: ExprId,
        count: ExprId,
    },

    // Struct literal
    // .{x: 10, y: 20}
    StructLit {
        path: Option<PathId>,
        fields: Vec<FieldInit>,
    },

    // Enum literal (Zig-style)
    // Result{Ok: 5}, Result{Err: .{x: 56, y: "hello"}}
    EnumLit {
        path: Option<PathId>,
        field: FieldInit,
    },

    // Tuple literal
    // .{42, 3.14}
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

    // Optional operators
    Unwrap(ExprId), // x!
    UnwrapOr {
        // x ?? default
        expr: ExprId,
        default: ExprId,
    },
    OptionalMap(ExprId), // x?

    // Range
    Range {
        start: Option<ExprId>,
        end: Option<ExprId>,
        inclusive: bool,
    },

    // Assignment
    Assign {
        op: AssignOp,
        target: ExprId,
        value: ExprId,
    },

    // Type cast
    Cast {
        expr: ExprId,
        ty: TypeId,
    },

    // Control flow
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

    // Jump statements
    Return(ExprId),
    Break(Option<ExprId>),
    Continue,

    // Function call
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },

    // Field access
    Field {
        object: ExprId,
        field: Ident,
    },

    // Index
    Index {
        object: ExprId,
        index: ExprId,
    },

    // Comptime expression (forces compile-time evaluation)
    Comptime(ExprId),
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub name: Ident,
    pub value: Option<ExprId>,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,    // -
    Not,    // !
    BitNot, // ~
    Ref,    // &
    RefMut, // &mut
    Deref,  // * (prefix)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,

    // Comparison
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    // Logical
    And,
    Or,
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
    // Empty statement
    Semi,

    // Variable declaration
    VarDecl {
        mutable: bool,
        pattern: PatternId,
        ty: Option<TypeId>,
        value: ExprId,
    },

    // Comptime variable declaration
    ConstVarDecl {
        mutable: bool, // NEW: comptime var vs comptime val
        pattern: PatternId,
        ty: Option<TypeId>,
        value: ExprId,
    },

    // Expression statement
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
    // Wildcard pattern: _
    Wildcard,

    // Variable binding: x
    Variable {
        mutable: bool,
        name: Ident,
    },

    Path(PathId),

    // Literal patterns
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(StrSymbol),

    Struct {
        path: Option<PathId>,
        fields: Vec<NamedFieldPattern>,
    },

    Tuple {
        elements: Vec<PatternId>,
    },

    Enum {
        path: Option<PathId>,
        variant: Ident,
        payload: Option<PatternId>,
    },

    Lit(ExprId),

    // Or pattern: a | b
    Or(Vec<PatternId>),

    // Range pattern: 0..10
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
// Items (top-level declarations)
// ============================================================================

#[derive(Debug, Clone)]
pub struct Item {
    pub visibility: Visibility,
    pub name: Ident,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    // Module
    Module {
        items: Vec<ItemId>,
    },

    // Function
    Function {
        generics: Generics,
        params: Vec<Param>,
        ret: TypeId,
        body: ExprId,
        is_const: bool, // const fn = comptime function
    },

    // Structs and Enums (always named fields)
    DataType {
        is_enum: bool,
        generics: Generics,
        fields: Vec<Field>,
        methods: Vec<ItemId>,
    },

    // Constant
    Const {
        generics: Generics,
        ty: Option<TypeId>,
        value: ExprId,
    },
}

// ============================================================================
// Generics
// ============================================================================

#[derive(Clone, Debug, Default)]
pub struct Generics {
    pub params: Vec<GenericParam>,
}

impl Generics {
    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }
}

// Generic parameter: comptime parameter with a type
// T: type, N: int, etc.
#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: Ident,
    pub ty: TypeId,
    pub span: Span,
}

// ============================================================================
// Function parameters
// ============================================================================

#[derive(Debug, Clone)]
pub struct Param {
    pub mutable: bool,
    pub name: Ident,
    pub ty: TypeId,
    pub span: Span,
}

// ============================================================================
// Paths
// ============================================================================

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub ident: Ident,
    pub span: Span,
}

impl Type {
    pub fn name(&self, interner: &Interner, ast: &AstArena) -> String {
        match &self.kind {
            TypeKind::Any => "any".to_string(),
            TypeKind::Int => "int".to_string(),
            TypeKind::Float => "float".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::String => "string".to_string(),
            TypeKind::Char => "char".to_string(),
            TypeKind::Void => "void".to_string(),
            TypeKind::Never => "!".to_string(),
            TypeKind::Type => "type".to_string(),
            TypeKind::Optional(ty) => format!("{}?", ast.types[*ty].name(interner, ast)),
            TypeKind::DynArray(ty) => format!("[{}]", ast.types[*ty].name(interner, ast)),
            TypeKind::Array { element, .. } => {
                format!("[{}; _]", ast.types[*element].name(interner, ast))
            }
            TypeKind::Pointer { pointee, mutable } => {
                let prefix = if *mutable { "@mut " } else { "@" };
                format!("{}{}", prefix, ast.types[*pointee].name(interner, ast))
            }
            TypeKind::Function { params, ret } => {
                let params_str = params
                    .iter()
                    .map(|p| ast.types[*p].name(interner, ast))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "fn({}) -> {}",
                    params_str,
                    ast.types[*ret].name(interner, ast)
                )
            }
            TypeKind::AnonStruct { .. } => "${ ... }".to_string(),
            TypeKind::Tuple { .. } => "${ ... }".to_string(),
            TypeKind::AnonEnum { .. } => "#{ ... }".to_string(),
            TypeKind::Range { element, inclusive } => {
                let op = if *inclusive { "..=" } else { ".." };
                format!("{}{}", ast.types[*element].name(interner, ast), op)
            }
            TypeKind::Infer => "_".to_string(),
            TypeKind::Path(_) => "<path>".to_string(),
        }
    }
}
