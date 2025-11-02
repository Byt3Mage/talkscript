use crate::{
    token::Span,
    vm::vm_types::{Boolean, Float, Integer},
};

type Symbol = String;

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundOp {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Int(Integer),
    Float(Float),
    Bool(Boolean),
    String(Symbol),
    Null,
    Void,
    Ident(Symbol),
    Group(Box<Expr>),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    CompoundAssign {
        op: CompoundOp,
        target: Box<Expr>,
        value: Box<Expr>,
    },

    Continue,
    Return(Option<Box<Expr>>),
    Break(Option<Box<Expr>>),
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Block {
        statements: Vec<Stmt>,
        trailing_expr: Option<Box<Expr>>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
}

impl Expr {
    pub fn is_assignable(&self) -> bool {
        match &self.kind {
            ExprKind::Ident(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Semi,
    Let {
        name: Symbol,
        ty: Option<Type>,
        value: Expr,
    },
    Expr(Expr),
}

#[derive(Debug)]
pub enum Pattern {
    Wildcard,
    Variable(Symbol),
    Destructure {
        base: Symbol,
        fields: Vec<(Symbol, Pattern)>,
    },
    Tuple(Vec<Pattern>),
    // Possibly: Or(Pattern, Pattern), Ref(Box<Pattern>), etc.
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>, // block or expr
}

#[derive(Debug)]
pub struct Param {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Field {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Int,
    Float,
    Bool,
    String,
    Void,
    Never,
    Optional(Box<Self>),
    Function {
        params: Vec<Box<Self>>,
        return_ty: Box<Self>,
    },
    Symbol(Symbol),
}

#[derive(Debug)]
pub enum ItemKind {
    Function {
        name: Symbol,
        params: Vec<Param>,
        ret: Type,
        body: Expr,
    },
    Struct {
        name: Symbol,
        fields: Vec<Field>,
    },
}

#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}
