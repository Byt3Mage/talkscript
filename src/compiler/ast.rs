use crate::{
    arena::{Arena, Ident, StrSymbol},
    compiler::tokens::Span,
};

slotmap::new_key_type! {
    pub struct ExprId;
    pub struct StmtId;
    pub struct ItemId;
    pub struct PatternId;
    pub struct AstTypeId;
}

pub struct AstArena {
    pub exprs: Arena<ExprId, Expr>,
    pub stmts: Arena<StmtId, Stmt>,
    pub items: Arena<ItemId, Item>,
    pub types: Arena<AstTypeId, AstType>,
    pub patterns: Arena<PatternId, Pattern>,
}

impl AstArena {
    pub fn new() -> Self {
        Self {
            exprs: Arena::with_key(),
            stmts: Arena::with_key(),
            items: Arena::with_key(),
            types: Arena::with_key(),
            patterns: Arena::with_key(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum GenericArg {
    Type(AstTypeId),
    Const(ExprId),
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub name: Ident,
    pub args: Vec<GenericArg>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstType {
    pub kind: AstTypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum AstTypeKind {
    // int, float, std::collections::List, List<int>
    Path(Path),

    // @T, @mut T
    Pointer {
        pointee: AstTypeId,
        mutable: bool,
    },

    // ?T
    Optional(AstTypeId),

    // [T; N]
    Array {
        elem: AstTypeId,
        size: ExprId,
    },

    // [T]
    Slice(AstTypeId),

    // (A, B, C)
    Tuple(Vec<AstTypeId>),

    // fn(A, B) -> C
    Function {
        params: Vec<AstTypeId>,
        ret: AstTypeId,
    },

    // Self
    SelfType,

    // _
    Infer,

    // struct { x: int, y: int }
    AnonStruct(Vec<Field>),

    // union { ok: int, err: cstr }
    AnonUnion(Vec<Field>),

    // enum { Red, Green, Blue }
    AnonEnum(Vec<Variant>),
}

#[derive(Clone, Debug, Default)]
pub struct GenericParams {
    pub params: Vec<GenericParam>,
}

impl GenericParams {
    pub fn new() -> Self {
        Self { params: vec![] }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: Ident,
    pub kind: GenericParamKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum GenericParamKind {
    // <T>, <T = int>
    Type {
        default: Option<AstTypeId>,
    },

    // <const N: int>, <const N: int = 10>
    Const {
        ty: AstTypeId,
        default: Option<ExprId>,
    },
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Uint(u64),
    Float(f64),
    Bool(bool),
    Char(char),
    Cstr(StrSymbol),
    Void,

    // x, std::math::sqrt, Option::<int>
    Path(Path),

    Group(ExprId),

    ArrayLit(Vec<ExprId>),
    ArrayRepeat {
        value: ExprId,
        count: ExprId,
    },

    // Point { x: 10, y: 20 }
    // std::geo::Point::<int> { x: 10, y: 20 }
    StructLit {
        path: Path,
        fields: Vec<FieldInit>,
    },

    // .{ x: 10, y: 20 }
    AnonStructLit {
        fields: Vec<FieldInit>,
    },

    // Option::<int> { some: 42 }
    UnionLit {
        path: Path,
        field: FieldInit,
    },

    // .{ ok: 42 }
    AnonUnionLit {
        field: FieldInit,
    },

    TupleLit(Vec<ExprId>),

    Unary {
        op: UnaryOp,
        expr: ExprId,
    },
    Binary {
        op: BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },

    Assign {
        op: AssignOp,
        target: ExprId,
        value: ExprId,
    },

    Cast {
        expr: ExprId,
        ty: AstTypeId,
    },

    If {
        cond: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    },
    Match {
        scrutinee: ExprId,
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

    Block(Vec<StmtId>),

    Return(Option<ExprId>),
    Break(Option<ExprId>),
    Continue,

    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },

    MethodCall {
        receiver: ExprId,
        method: Ident,
        type_args: Vec<AstTypeId>,
        args: Vec<ExprId>,
    },

    Field {
        object: ExprId,
        field: Ident,
    },

    OptionalChain {
        object: ExprId,
        field: Ident,
    },

    Index {
        object: ExprId,
        index: ExprId,
    },

    Range {
        start: Option<ExprId>,
        end: Option<ExprId>,
        inclusive: bool,
    },

    Unwrap(ExprId),

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
    Neg,
    Not,
    BitNot,
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
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
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
    pub span: Span,
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
    // let x = 5;
    // let x: int = 5;
    // let mut x = 5;
    Let {
        pattern: PatternId,
        ty: Option<AstTypeId>,
        value: ExprId,
    },

    Expr {
        expr: ExprId,
        has_semi: bool,
    },

    Empty,
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

    Binding {
        mutable: bool,
        name: Ident,
    },

    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    CStr(StrSymbol),
    Path(Path),

    // Point { x, y }
    // std::geo::Point { x, y, .. }
    Struct {
        path: Path,
        fields: Vec<FieldPattern>,
        rest: bool,
    },

    // .{ x, y }
    // .{ x, y, ..}
    AnonStruct {
        fields: Vec<FieldPattern>,
        rest: bool,
    },

    // Option::<int> { some: x }
    Union {
        path: Path,
        field: FieldPattern,
    },

    // .{ ok: x }
    AnonUnion {
        field: FieldPattern,
    },

    Tuple(Vec<PatternId>),

    Array(Vec<PatternId>),

    Or(Vec<PatternId>),

    Range {
        start: Option<ExprId>,
        end: Option<ExprId>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone)]
pub struct FieldPattern {
    pub name: Ident,
    pub pattern: PatternId,
    pub span: Span,
}

// ============================================================================
// Items (Top-level Declarations)
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
    // mod foo { ... }
    Module {
        items: Vec<ItemId>,
    },

    // fn foo<T>(x: T) -> T { ... }
    Function {
        generics: GenericParams,
        params: Vec<Param>,
        ret: Option<AstTypeId>,
        body: ExprId,
    },

    // struct Point { x: int, y: int }
    // struct Pair<T, U> { first: T, second: U }
    Struct {
        generics: GenericParams,
        fields: Vec<Field>,
    },

    // enum Color { Red = 0, Green, Blue }
    Enum {
        variants: Vec<Variant>,
    },

    // union Option<T> { some: T, none: void }
    // union Result<T, E> { ok: T, err: E }
    Union {
        generics: GenericParams,
        fields: Vec<Field>,
    },

    // const PI: float = 3.14159;
    Const {
        ty: Option<AstTypeId>,
        value: ExprId,
    },

    // type StringList = List<str>;
    TypeAlias {
        generics: GenericParams,
        ty: AstTypeId,
    },

    // import std::math;
    // import {math, constants} from std;
    Import(Import),
}

// ============================================================================
// Import
// ============================================================================

#[derive(Debug, Clone)]
pub struct Import {
    pub kind: ImportKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    // import std::math;
    Simple(Path),

    // import {math, constants} from std;
    From { imports: Vec<Path>, source: Path },
}

// ============================================================================
// Item Components
// ============================================================================

#[derive(Debug, Clone)]
pub struct Param {
    pub pattern: PatternId,
    pub ty: AstTypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub visibility: Visibility,
    pub name: Ident,
    pub ty: AstTypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Ident,
    pub value: Option<ExprId>,
    pub span: Span,
}

// ============================================================================
// Helpers
// ============================================================================

impl Expr {
    pub fn is_place(&self) -> bool {
        matches!(
            self.kind,
            ExprKind::Path(_)
                | ExprKind::Field { .. }
                | ExprKind::Index { .. }
                | ExprKind::Unary {
                    op: UnaryOp::Deref,
                    ..
                }
        )
    }
}

impl Path {
    pub fn single(name: Ident, span: Span) -> Self {
        Self {
            segments: vec![PathSegment {
                name,
                args: vec![],
                span,
            }],
            span,
        }
    }

    pub fn is_single(&self) -> bool {
        self.segments.len() == 1 && self.segments[0].args.is_empty()
    }

    pub fn as_single(&self) -> Option<Ident> {
        self.is_single().then(|| self.segments[0].name)
    }
}
