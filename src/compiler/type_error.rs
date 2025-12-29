use crate::{
    arena::Ident,
    compiler::{tokens::Span, type_info::TypeId},
};

#[derive(Debug, Clone)]
pub enum TypeError {
    TypeMismatch {
        expected: TypeId,
        found: TypeId,
        span: Span,
    },
    UndefinedType {
        name: Ident,
        span: Span,
    },
    NotAType {
        span: Span,
    },
    NotAStruct {
        ty: TypeId,
        span: Span,
    },
    NotAUnion {
        ty: TypeId,
        span: Span,
    },
    WrongNumberOfTypeArgs {
        expected: usize,
        found: usize,
        span: Span,
    },
    InvalidArrayLength {
        span: Span,
    },
    FieldNotFound {
        ty: TypeId,
        field: Ident,
        span: Span,
    },
    NotCallable {
        ty: TypeId,
        span: Span,
    },
    DuplicateEnumValue {
        name: Ident,
        value: i64,
        first_def: Span,
        dupe_def: Span,
    },
    SelfOutsideImpl {
        span: Span,
    },
    CannotInfer {
        span: Span,
    },
    GenericParamWithArgs {
        span: Span,
    },
    NotAModule {
        span: Span,
    },
    TypeArgsOnModule {
        span: Span,
    },
    TypeArgsOnConst {
        span: Span,
    },
    NotConstExpr {
        span: Span,
    },
    InvalidConstOp {
        span: Span,
    },
    DivisionByZero {
        span: Span,
    },
    TypeJoinInvalid {
        first: TypeId,
        second: TypeId,
        span: Span,
    },
    NotImplemented {
        span: Span,
    },
    UndefinedVariable {
        name: Ident,
        span: Span,
    },
    TypeUsedAsValue {
        span: Span,
    },
}
