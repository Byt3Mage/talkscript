use crate::arena::{Arena, Interner, StrSymbol};

use super::tokens::Span;

pub type Ident = StrSymbol;

slotmap::new_key_type! {
    pub struct ExprId;
    pub struct StmtId;
    pub struct ItemId;
    pub struct TypeId;
    pub struct PathId;
}

pub struct AstArena {
    pub exprs: Arena<ExprId, Expr>,
    pub stmts: Arena<StmtId, Stmt>,
    pub items: Arena<ItemId, Item>,
    pub types: Arena<TypeId, Type>,
    pub paths: Arena<PathId, Path>,
}

impl AstArena {
    pub fn new() -> Self {
        Self {
            exprs: Arena::with_key(),
            stmts: Arena::with_key(),
            items: Arena::with_key(),
            types: Arena::with_key(),
            paths: Arena::with_key(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
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

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Equal => "=",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Greater => ">",
            BinaryOp::Less => "<",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::LessEqual => "<=",
        };

        f.write_str(s)
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(StrSymbol),
    ArrayLit(Vec<ExprId>),
    Null,
    Void,
    Ident(Ident),
    Group(ExprId),
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
        tgt: ExprId,
        val: ExprId,
    },
    Continue,
    Return(ExprId), // return; => return void;
    Break(ExprId),  // break; => break void;
    If {
        cond: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    },
    Block(Vec<StmtId>),
    While {
        cond: ExprId,
        body: ExprId,
    },
    Match {
        expr: ExprId,
        arms: Vec<MatchArm>,
    },
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    Field {
        object: ExprId,
        field: Ident,
    },
    Index {
        object: ExprId,
        index: ExprId,
    },
    Loop(ExprId),
}

impl Expr {
    pub fn is_assignable(&self) -> bool {
        matches!(
            self.kind,
            ExprKind::Ident(_) | ExprKind::Field { .. } | ExprKind::Index { .. }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Semi,
    VarDecl {
        mutable: bool,
        name: Ident,
        ty: Option<TypeId>,
        val: ExprId,
    },
    Expr {
        expr: ExprId,
        has_semi: bool,
    },
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Wildcard,
    Variable(Ident),
    Destructure {
        base: Ident,
        fields: Vec<(Ident, Pattern)>,
    },
    Tuple(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<ExprId>,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub mutable: bool,
    pub name: Ident,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: Ident,
    pub default: Option<TypeId>,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct Generics {
    pub params: Vec<GenericParam>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: TypeId,
    pub span: Span,
}

pub struct Path {
    prefix: Box<[Ident]>,
    last: Ident,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Any,
    Int,
    Float,
    Bool,
    String,
    Void,
    Never,
    Optional(TypeId),
    Array { ty: TypeId, len: ExprId },
    DynArray(TypeId),
    Function { params: Vec<TypeId>, ret: TypeId },
    Path(Ident),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Module {
        items: Vec<ItemId>,
    },
    Function {
        params: Vec<Param>,
        ret: TypeId,
        body: ExprId,
    },
    Struct {
        fields: Vec<Field>,
    },
}

#[derive(Debug, Clone)]
pub struct Item {
    pub is_pub: bool, // TODO: expand to parent, etc.
    pub name: Ident,
    pub kind: ItemKind,
    pub span: Span,
}

impl Type {
    pub fn name(&self, interner: &Interner, ast: &Arena<TypeId, Type>) -> String {
        match &self.kind {
            TypeKind::Any => "any".to_string(),
            TypeKind::Int => "int".to_string(),
            TypeKind::Float => "float".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::String => "string".to_string(),
            TypeKind::Void => "void".to_string(),
            TypeKind::Never => "!".to_string(),
            TypeKind::Optional(ty) => format!("{}?", ast[*ty].name(interner, ast)),
            TypeKind::DynArray(ty) => format!("[{}]", ast[*ty].name(interner, ast)),
            TypeKind::Array { ty, .. } => format!("[{}; _]", ast[*ty].name(interner, ast)),
            TypeKind::Function { params, ret } => {
                let params_str = params
                    .iter()
                    .map(|p| ast[*p].name(interner, ast))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("fn({}) -> {}", params_str, ast[*ret].name(interner, ast))
            }
            TypeKind::Path(_) => todo!("path name"),
        }
    }
}
