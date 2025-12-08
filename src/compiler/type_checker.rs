use ahash::AHashMap;

use crate::{
    arena::Interner,
    compiler::{
        ast::{
            AssignOp, AstArena, BinaryOp, Expr, ExprKind, Ident, Item, ItemKind, Stmt, StmtKind, Type, TypeId, Type, UnaryOp
        },
        resolver::{SymbolKind, SymbolTable},
        tokens::Span,
    },
};

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    UndefinedVariable(Ident),
    TypeMismatch {
        exp: TypeId,
        got: TypeId,
    },
    CannotInferType {
        reason: String,
    },
    InvalidUnaryOp {
        op: UnaryOp,
        operand_ty: TypeId,
    },
    InvalidBinaryOp {
        op: String,
        lhs_ty: String,
        rhs_ty: String,
    },
    ArgumentCountMismatch {
        exp: usize,
        got: usize,
    },
    ArgumentTypeMismatch {
        arg_index: usize,
        exp: TypeId,
        got: TypeId,
    },
    NotCallable {
        ty: TypeId,
    },
    NoSuchField {
        type_name: Ident,
        field_name: Ident,
    },
    TypeHasNoFields {
        ty: TypeId,
    },
    NotIndexable {
        ty: TypeId,
    },
    IndexMustBeInt {
        found: TypeId,
    },
    BreakOutsideLoop,
    BreakWithValueInWhile,
    ContinueOutsideLoop,
    ReturnTypeMismatch {
        exp: TypeId,
        got: TypeId,
    },
    ConditionMustBeBool {
        got: String,
    },
    IfBranchTypeMismatch {
        then_ty: TypeId,
        else_ty: TypeId,
    },
    IfWithoutElseMustBeVoid {
        found: TypeId,
    },
    InconsistentBreakTypes,
    VarNeedsTypeOrInit {
        name: Ident,
    },
    AssignmentTypeMismatch {
        tgt_ty: TypeId,
        val_ty: TypeId,
    },
    NotAValue {
        name: Ident,
    },
    UnknownStruct {
        name: Ident,
    },
    EmptyArrayLiteral,
    MixedArrayTypes,
    TypeCheckingFailed,
}

#[derive(Debug, Clone)]
pub struct TypeError {
    kind: TypeErrorKind,
    span: Span,
}

impl TypeError {
    pub fn display(&self, interner: &Interner, ast: &AstArena) -> String {
        match &self.kind {
            TypeErrorKind::UndefinedVariable(name) => {
                let name_str = interner.resolve(*name).unwrap_or("?");
                format!("Undefined variable '{}'", name_str)
            }
            TypeErrorKind::TypeMismatch { exp, got } => {
                let exp = ast.types[*exp].name(interner, &ast.types);
                let got = ast.types[*got].name(interner, &ast.types);
                format!("Type mismatch: expected {exp}, found {got}")
            }
            TypeErrorKind::CannotInferType { reason } => {
                format!("Cannot infer type: {reason}")
            }
            TypeErrorKind::InvalidUnaryOp { op, operand_ty } => {

                format!("Cannot apply operator {op} to type {operand_ty}")
            }
            TypeErrorKind::InvalidBinaryOp {
                op, lhs_ty, rhs_ty, ..
            } => {
                format!("Cannot apply operator {op} to types {lhs_ty} and {rhs_ty}")
            }
            TypeErrorKind::ArgumentCountMismatch { exp, got } => {
                format!("Expected {exp} arguments, found {got}")
            }
            TypeErrorKind::ArgumentTypeMismatch {
                arg_index,
                exp,
                got,
            } => {
                format!("Argument {arg_index} has type {got}, expected {exp}")
            }
            TypeErrorKind::NotCallable { ty, .. } => {
                format!("Cannot call non-function type {}", ty)
            }
            TypeErrorKind::NoSuchField {
                type_name,
                field_name,
            } => {
                let type_str = interner.resolve(*type_name).unwrap_or("?");
                let field_str = interner.resolve(*field_name).unwrap_or("?");
                format!("Type '{type_str}' has no field '{field_str}'")
            }
            TypeErrorKind::TypeHasNoFields { ty } => {
                format!("Type {ty} has no fields")
            }
            TypeErrorKind::NotIndexable { ty } => {
                format!("Cannot index type {ty}")
            }
            TypeErrorKind::IndexMustBeInt { found } => {
                format!("Index must be int, found {found}",)
            }
            TypeErrorKind::BreakOutsideLoop => "Break statement outside of loop".to_string(),
            TypeErrorKind::BreakWithValueInWhile => {
                "Cannot break with value in while loop (only 'loop' supports break values)"
                    .to_string()
            }
            TypeErrorKind::ContinueOutsideLoop => "Continue statement outside of loop".to_string(),
            TypeErrorKind::ReturnTypeMismatch { exp, got } => {
                format!("Return type mismatch: expected {exp}, found {got}")
            }
            TypeErrorKind::ConditionMustBeBool { got } => {
                format!("Condition must be bool, found {got}")
            }
            TypeErrorKind::IfBranchTypeMismatch {
                then_ty, else_ty, ..
            } => {
                format!(
                    "If branches have different types: {} and {}",
                    then_ty, else_ty
                )
            }
            TypeErrorKind::IfWithoutElseMustBeVoid { found } => {
                format!("If without else must have void body, found {found}")
            }
            TypeErrorKind::InconsistentBreakTypes => {
                "Loop has inconsistent break types".to_string()
            }
            TypeErrorKind::VarNeedsTypeOrInit { name } => {
                let name_str = interner.resolve(*name).unwrap_or("?");
                format!("Variable '{name_str}' must have either a type annotation or initializer")
            }
            TypeErrorKind::AssignmentTypeMismatch { tgt_ty, val_ty } => {
                format!("Cannot assign {val_ty} to {tgt_ty}")
            }
            TypeErrorKind::NotAValue { name } => {
                let name_str = interner.resolve(*name).unwrap_or("?");
                format!("'{name_str}' is not a value")
            }
            TypeErrorKind::UnknownStruct { name } => {
                let name_str = interner.resolve(*name).unwrap_or("?");
                format!("Unknown struct '{}'", name_str)
            }
            TypeErrorKind::EmptyArrayLiteral => {
                "Cannot infer type of empty array literal".to_string()
            }
            TypeErrorKind::MixedArrayTypes => {
                "Array literal has elements of different types".to_string()
            }
            TypeErrorKind::TypeCheckingFailed => {
                "Type checker must start at global scope".to_string()
            }
        }
    }
}
pub struct TypeChecker<'a> {
    interner: &'a Interner,
    symbols: SymbolTable,
    errors: Vec<TypeError>,
    /// Map from expression spans to their inferred types (for later passes)
    expr_types: AHashMap<Span, Type>,
    /// Current function return type (for checking return statements)
    current_fn_ret: Option<Type>,
    /// Are we inside a loop? (for checking break/continue)
    break_stack: Vec<Option<Vec<Type>>>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(interner: &'a Interner, symbols: SymbolTable) -> Self {
        Self {
            interner,
            symbols,
            errors: vec![],
            expr_types: AHashMap::new(),
            current_fn_ret: None,
            break_stack: vec![],
        }
    }

    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn expr_types(&self) -> &AHashMap<Span, Type> {
        &self.expr_types
    }

    fn check_package(&mut self, items: &[Item], span: Bbboo) -> Result<(), TypeError> {
        if self.symbols.current_scope() != 0 {
            return Err(TypeError {
                kind: TypeErrorKind::TypeCheckingFailed,
                span,
            });
        }

        for item in items {
            self.check_item(item);
        }

        if self.symbols.current_scope() != 0 {
            return Err(TypeError {
                kind: TypeErrorKind::TypeCheckingFailed,
                span,
            });
        }

        Ok(())
    }

    pub fn check_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Module { items, name } => {
                let module_scope = 0; //todo: get scope

                self.symbols.set_current_scope(module_scope);

                for item in items {
                    self.check_item(item);
                }

                if let Some(parent) = self.symbols.scopes[module_scope].parent {
                    self.symbols.set_current_scope(parent);
                }
            }
            ItemKind::Function {
                name,
                params,
                ret,
                body,
            } => {
                let function_scope = 0; //todo: get scope

                self.current_fn_ret = Some(ret.clone());

                self.symbols.set_current_scope(function_scope);

                match self.infer_expr(body) {
                    Ok(body_ty) => {
                        if !self.types_compatible(&body_ty, ret) {
                            self.errors.push(TypeErrorKind::ReturnMismatch {
                                func_name: *name,
                                exp: ret.clone(),
                                got: body_ty.clone(),
                            })
                        }
                    }
                    Err(e) => self.errors.push(e),
                }

                self.current_fn_ret = None;
            }
            ItemKind::Struct { .. } => { /*Already validated in resolver */ }
        }
    }

    pub fn infer_expr(&mut self, expr: &Expr, ast: &mut AstArena) -> Result<Type, TypeError> {
        let ty = match &expr.kind {
            ExprKind::Int(_) => Type {
                kind: Type::Int,
                span: expr.span,
            },
            ExprKind::Float(_) => Type {
                kind: Type::Float,
                span: expr.span,
            },
            ExprKind::Bool(_) => Type {
                kind: Type::Bool,
                span: expr.span,
            },
            ExprKind::String(_) => Type {
                kind: Type::String,
                span: expr.span,
            },
            ExprKind::Null => Type {
                kind: Type::Optional(ast.types.insert(Type {
                    kind: Type::Unknown,
                    span: expr.span,
                })),
                span: expr.span,
            },
            ExprKind::Void => Type {
                kind: Type::Void,
                span: expr.span,
            },
            ExprKind::Ident(name) => {
                if let Some(symbol) = self.symbols.lookup(name) {
                    match &symbol.kind {
                        SymbolKind::Variable { ty, .. } => ast.types[*ty].clone(),
                        SymbolKind::Function { params, ret, .. } => Type {
                            kind: Type::Function {
                                params: params.clone(),
                                ret: ret.clone(),
                            },
                            span: expr.span,
                        },
                        _ => return Err(format!("'{name}' is not a value")),
                    }
                } else {
                    return Err(format!("Undefined variable '{name}'"));
                }
            }
            ExprKind::Group(expr) => self.infer_expr(expr, scope)?,
            ExprKind::Unary { op, expr } => {
                let ty = self.infer_expr(&ast.)?;
                match (op, ty.kind) {
                    (UnaryOp::Neg, Type::Int | Type::Float) => Ok(ty.clone()),
                    (UnaryOp::Not, Type::Bool) => Ok(ty.clone()),
                    _ => Err(TypeError {
                        kind: todo!(),
                        span: todo!(),
                    })
                }
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lhs_ty = self.infer_expr(lhs, scope)?;
                let rhs_ty = self.infer_expr(rhs, scope)?;
                self.check_binary_op(*op, &lhs_ty, &rhs_ty)?
            }
            ExprKind::Assign { op, tgt, val } => {
                let tgt_ty = self.infer_expr(tgt, scope)?;
                let val_ty = self.infer_expr(val, scope)?;

                if !self.types_compatible(&val_ty, &tgt_ty) {
                    return Err(format!("Cannot assign {val_ty} to {tgt_ty}"));
                }

                if *op != AssignOp::Assign {
                    match *op {
                        AssignOp::AddAssign => {
                            self.check_binary_op(BinaryOp::Add, &tgt_ty, &val_ty)?
                        }
                        AssignOp::SubAssign => {
                            self.check_binary_op(BinaryOp::Sub, &tgt_ty, &val_ty)?
                        }
                        AssignOp::MulAssign => {
                            self.check_binary_op(BinaryOp::Mul, &tgt_ty, &val_ty)?
                        }
                        AssignOp::DivAssign => {
                            self.check_binary_op(BinaryOp::Div, &tgt_ty, &val_ty)?
                        }
                        AssignOp::ModAssign => {
                            self.check_binary_op(BinaryOp::Mod, &tgt_ty, &val_ty)?
                        }
                        AssignOp::Assign => unreachable!(),
                    };
                }

                Type {
                    kind: Box::new(Type::Void),
                    span: expr.span,
                }
            }
            ExprKind::Call { callee, args } => {
                let callee_ty = self.infer_expr(callee, scope)?;

                match &*callee_ty.kind {
                    Type::Function { params, ret: return_ty } => {
                        if args.len() != params.len() {
                            return Err(format!(
                                "Expected {} arguments, got {}",
                                params.len(),
                                args.len()
                            ));
                        }

                        for (i, (arg, param_ty)) in args.iter().zip(params.iter()).enumerate() {
                            let arg_ty = self.infer_expr(arg, scope)?;
                            if !self.types_compatible(&arg_ty, param_ty) {
                                return Err(format!(
                                    "Argument {i} has type {arg_ty}, expected {param_ty}"
                                ));
                            }
                        }

                        return_ty.clone()
                    }
                    _ => return Err(format!("Cannot call non-function type {callee_ty}")),
                }
            }
            ExprKind::Field { object, field } => {
                let object_ty = self.infer_expr(object, scope)?;

                match &*object_ty.kind {
                    Type::Symbol(name) => {
                        if let Some(symbol) = self.symbols.lookup(name) {
                            if let SymbolKind::Struct { fields } = &symbol.kind {
                                for f in fields {
                                    if &f.name == field {
                                        return Ok(f.ty.clone());
                                    }
                                }

                                return Err(format!("Struct '{name}' has no field '{field}'"));
                            }
                        }
                        return Err(format!("Unknown struct '{name}'"));
                    }
                    _ => return Err(format!("Type {object_ty} has no fields")),
                }
            }
            ExprKind::Index { object, index } => {
                let object_ty = self.infer_expr(object, scope)?;
                let index_ty = self.infer_expr(index, scope)?;

                // Index must be int
                if !matches!(*index_ty.kind, Type::Int) {
                    return Err(format!("Index must be int, got {index_ty}"));
                }

                // Check object is indexable
                match &*object_ty.kind {
                    Type::Array { ty, .. } | Type::DynArray(ty) => ty.clone(),
                    _ => {
                        return Err(format!("Cannot index type {object_ty}"));
                    }
                }
            }
            ExprKind::ArrayLit(elements) => todo!("infer array lit"),
            ExprKind::Block(stmts) => self.check_block(stmts)?,
            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.infer_expr(cond, scope)?;

                if !matches!(*cond_ty.kind, Type::Bool) {
                    return Err(format!("If condition must be gool, got {cond_ty}"));
                }

                let then_ty = self.infer_expr(then_branch, scope)?;

                if let Some(else_expr) = else_branch {
                    let else_ty = self.infer_expr(else_expr, scope)?;

                    if !self.types_compatible(&then_ty, &else_ty) {
                        return Err(format!(
                            "If branches have different types: {then_ty} and {else_ty}"
                        ));
                    }
                } else {
                    if !matches!(*then_ty.kind, Type::Void) {
                        return Err(format!(
                            "If without else must have void body, got {then_ty}"
                        ));
                    }
                }

                then_ty
            }
            ExprKind::While { cond, body } => {
                let cond_ty = self.infer_expr(cond)?;
                if !matches!(*cond_ty.kind, Type::Bool) {
                    return Err(format!(
                        "While condition must be bool, got {:?}",
                        cond_ty.kind
                    ));
                }

                self.loop_depth += 1;
                let body_ty = self.infer_expr(body)?;
                self.loop_depth -= 1;

                // While always returns void
                Type {
                    kind: Box::new(Type::Void),
                    span: expr.span,
                }
            }
            ExprKind::Match { expr, arms } => todo!(),
            ExprKind::Loop(body) => {
                self.loop_depth += 1;
                let body_ty = self.infer_expr(body)?;
                self.loop_depth -= 1;

                // Loop returns Never (infinite loop) unless broken
                Type {
                    kind: Box::new(Type::Never),
                    span: expr.span,
                }
            }
            ExprKind::Return(expr) => {
                let ret_ty = self.infer_expr(expr)?;

                if let Some(expected) = &self.current_fn_ret {
                    if !self.types_compatible(&ret_ty, expected) {
                        return Err(format!(
                            "Return type {ret_ty} doesn't match function return type {expected}"
                        ));
                    }
                }

                // Return has type Never (diverges)
                Type {
                    kind: Box::new(Type::Never),
                    span: expr.span,
                }
            }
            ExprKind::Break(expr) => {
                if self.loop_depth == 0 {
                    return Err("Break outside of loop".to_string());
                }

                let _ = self.infer_expr(expr)?;

                Type {
                    kind: Box::new(Type::Never),
                    span: expr.span,
                }
            }
            ExprKind::Continue => {
                if self.loop_depth == 0 {
                    return Err("Continue outside of loop".to_string());
                }

                Type {
                    kind: Box::new(Type::Never),
                    span: expr.span,
                }
            }
        };

        self.expr_types.insert(expr.span, ty.clone());
        Ok(ty)
    }

    fn check_block(&mut self, stmts: &[Stmt]) -> Result<Type, TypeError> {
        let mut last_type = Type {
            kind: Type::Void,
            span: Span::default(),
        };

        for stmt in stmts {
            match &stmt.kind {
                StmtKind::Semi => {}
                StmtKind::VarDecl {
                    mutable,
                    name,
                    ty,
                    val: value,
                } => {
                    if ty.is_none() && value.is_none() {
                        return Err(TypeError {
                            kind: TypeErrorKind::VarNeedsTypeOrInit { name: *name },
                            span: stmt.span,
                        });
                    }

                    if let Some(val_expr) = value {
                        let val_ty = self.infer_expr(val_expr)?;

                        if let Some(annotated_ty) = ty {
                            if !self.types_compatible(&val_ty, annotated_ty) {
                                return Err(TypeError {
                                    kind: TypeErrorKind::TypeMismatch { exp: , got: () },
                                    span: stmt.span,
                                });

                                format!(
                                    "Variable '{name}' declared as {annotated_ty} but initialized with {val_ty}"
                                )
                            }
                        } else {
                            self.symbols.set_var_type(name, val_ty)?;
                        }
                    }
                }
                StmtKind::Expr { expr, has_semi } => {
                    last_type = self.infer_expr(expr)?;

                    if *has_semi {
                        last_type = Type {
                            kind: Box::new(Type::Void),
                            span: stmt.span,
                        }
                    }
                }
            }
        }

        Ok(last_type)
    }

    fn check_binary_op(&self, op: BinaryOp, lhs: &Type, rhs: &Type) -> Result<Type, String> {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                match (&*lhs.kind, &*rhs.kind) {
                    (Type::Int, Type::Int) => Ok(Type {
                        kind: Box::new(Type::Int),
                        span: lhs.span,
                    }),
                    (Type::Float, Type::Float) => Ok(Type {
                        kind: Box::new(Type::Float),
                        span: lhs.span,
                    }),
                    _ => Err(format!("Cannot apply {op} to types {lhs} and {rhs}")),
                }
            }

            BinaryOp::Equal | BinaryOp::NotEqual => {
                if self.types_compatible(lhs, rhs) {
                    //TODO: check both types can be compared
                    Ok(Type {
                        kind: Box::new(Type::Bool),
                        span: lhs.span,
                    })
                } else {
                    Err(format!("Cannot compare types {lhs} and {rhs}"))
                }
            }

            BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual => {
                match (&*lhs.kind, &*rhs.kind) {
                    (Type::Int, Type::Int) | (Type::Float, Type::Float) => {
                        Ok(Type {
                            kind: Box::new(Type::Bool),
                            span: lhs.span,
                        })
                    }
                    _ => Err(format!("Cannot compare types {lhs} and {rhs}")),
                }
            }
        }
    }

    fn types_compatible(&self, actual: &Type, expected: &Type, ast: &AstArena) -> bool {
        match (actual, expected) {
            (Type::Unknown, _) | (_, Type::Unknown) => todo!("handle unknown type"),
            (Type::Any, _) | (_, Type::Any) => true,
            (Type::Never, _) => true,
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Void, Type::Void) => true,
            (Type::Path(a), Type::Path(b)) => todo!("resolve paths"),
            (Type::Optional(a), Type::Optional(b)) => {
                self.types_compatible( &ast.types[*a], &ast.types[*b], ast)
            }
            (Type::Array { ty, len }, Type::Array { ty, len }) => todo!("handle array types"),
            (Type::DynArray(a), Type::DynArray(b)) => self.types_compatible(a, b),
            // TODO: Handle more complex type compatibility
            _ => false,
        }
    }
}
