use std::{collections::hash_map::Entry, mem, rc::Rc};

use ahash::AHashMap;
use simple_ternary::tnr;

use crate::{
    arena::{Ident, Interner},
    compiler::{
        canon_ast::{
            BinaryOp, CanonAstArena, DeclId, DeclKind, EnumVariant, ExprId, ExprKind, Field,
            PatternId, PatternKind, StmtId, StmtKind, UnaryOp,
        },
        comptime_value::{ComptimeFloat, ComptimeValue},
        resolver::{ScopeId, SymbolId, SymbolTable},
        type_info::{
            Declaration, EnumInfo, FieldInfo, FunctionInfo, ModuleInfo, ParamInfo, StructInfo,
            TypeArena, TypeId, TypeValue, UnionInfo, VariantInfo,
        },
    },
};

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Break(Option<ComptimeValue>),
    Continue,
    Return(ComptimeValue),
}

#[derive(Debug, Clone)]
pub enum ComptimeError {
    EvalLimitExceeded {
        limit: usize,
    },
    TypeMismatch {
        exp: &'static str,
        got: ComptimeValue,
    },
    DivisionByZero,
    UndefinedSymbol {
        name: Ident,
        scope: ScopeId,
    },
    UndefinedVariable {
        name: Ident,
    },
    SymbolNotDecl {
        name: Ident,
        scope: ScopeId,
    },
    NotAFunction {
        decl: DeclId,
    },
    NotCallable {
        value: ComptimeValue,
    },
    ArgumentCountMismatch {
        exp: usize,
        got: usize,
    },
    NotComptimeEvaluable {
        expr: ExprId,
    },
    NotAType {
        expr: ExprId,
    },
    NotAConstInt {
        expr: ExprId,
    },
    CannotMutateImmutable {
        name: Ident,
    },
    InvalidUnaryOp {
        op: UnaryOp,
        val: ComptimeValue,
    },
    InvalidBinaryOp {
        op: BinaryOp,
        lhs: ComptimeValue,
        rhs: ComptimeValue,
    },
    InvalidAssignmentTarget {
        tgt: ExprId,
    },
    UnsupportedPattern {
        pattern: PatternId,
    },
    DeclNotFound {
        type_id: TypeId,
        name: Ident,
    },
    NotANamespace {
        type_id: TypeId,
    },
    NotTypeScope {
        value: ComptimeValue,
        member: Ident,
    },
    UnwrapNone {
        expr: ExprId,
    },
    NegativeArraySize {
        size: i64,
    },
    DuplicateDiscriminant {
        first: Ident,
        dupe: Ident,
    },
    IndexOutOfBounds {
        idx: i64,
        len: i64,
    },
}

pub type ComptimeResult<T> = Result<T, ComptimeError>;

pub struct EvalContext {
    // Stack of scopes, each scope maps name -> (value, is_mutable)
    scopes: Vec<AHashMap<Ident, (ComptimeValue, bool)>>,
    // Current control flow state (break/continue/return)
    control_flow: Option<ControlFlow>,

    current_decl: Option<DeclId>,

    current_scope: ScopeId,
}

impl EvalContext {
    fn new(current_decl: Option<DeclId>, current_scope: ScopeId) -> Self {
        Self {
            scopes: vec![AHashMap::new()], // Start with one global scope
            control_flow: None,
            current_decl,
            current_scope,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(AHashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind(&mut self, name: Ident, value: ComptimeValue, mutable: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, (value, mutable));
        }
    }

    fn lookup(&self, name: Ident) -> Option<&(ComptimeValue, bool)> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(&name) {
                return Some(binding);
            }
        }

        None
    }

    fn update(&mut self, name: Ident, value: ComptimeValue) -> Result<(), ComptimeError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((old_value, mutable)) = scope.get_mut(&name) {
                if !*mutable {
                    return Err(ComptimeError::CannotMutateImmutable { name });
                }
                *old_value = value;
                return Ok(());
            }
        }

        Err(ComptimeError::UndefinedVariable { name })
    }
}

pub struct ComptimeEvaluator<'a> {
    ast: &'a CanonAstArena,
    interner: &'a mut Interner,
    symbol_table: &'a SymbolTable,

    /// Type arena for creating type values
    types: TypeArena,

    /// Generic instantiation cache: (callee, args) => result
    generic_cache: AHashMap<(ExprId, Rc<[ComptimeValue]>), ComptimeValue>,

    // Declaration cache: decl -> value
    decl_cache: AHashMap<DeclId, ComptimeValue>,

    // Built-in types
    builtin_types: AHashMap<Ident, TypeId>,

    type_to_expr: AHashMap<TypeId, ExprId>,

    // Evaluation context
    context: EvalContext,

    // Evaluation counter and limit
    eval_count: usize,
    eval_limit: usize,
}

impl<'a> ComptimeEvaluator<'a> {
    pub fn new(
        ast: &'a CanonAstArena,
        interner: &'a mut Interner,
        symbol_table: &'a SymbolTable,
        current_decl: Option<DeclId>,
        current_scope: ScopeId,
    ) -> Self {
        let mut evaluator = Self {
            ast,
            interner,
            symbol_table,
            types: TypeArena::new(),
            generic_cache: AHashMap::new(),
            decl_cache: AHashMap::new(),
            builtin_types: AHashMap::new(),
            type_to_expr: AHashMap::new(),
            context: EvalContext::new(current_decl, current_scope),
            eval_count: 0,
            eval_limit: 1_000_000,
        };

        let mut insert_builtin = |name: &'static str, ty: TypeValue| {
            let name = evaluator.interner.get_or_intern_static(name);
            let ty = evaluator.types.insert(ty);
            evaluator.builtin_types.insert(name, ty);
        };

        insert_builtin("type", TypeValue::Type);
        insert_builtin("int", TypeValue::Int);
        insert_builtin("uint", TypeValue::Uint);
        insert_builtin("float", TypeValue::Float);
        insert_builtin("bool", TypeValue::Bool);
        insert_builtin("cstr", TypeValue::Cstr);
        insert_builtin("str", TypeValue::Str);
        insert_builtin("char", TypeValue::Char);
        insert_builtin("void", TypeValue::Void);
        insert_builtin("never", TypeValue::Never);

        evaluator
    }

    fn check_eval_limit(&mut self) -> ComptimeResult<()> {
        self.eval_count += 1;
        if self.eval_count > self.eval_limit {
            Err(ComptimeError::EvalLimitExceeded {
                limit: self.eval_limit,
            })
        } else {
            Ok(())
        }
    }

    pub fn eval_comptime(&mut self, expr_id: ExprId) -> ComptimeResult<ComptimeValue> {
        self.check_eval_limit()?;

        // Early exit if we have a control flow interrupt
        if self.context.control_flow.is_some() {
            return Ok(ComptimeValue::Void);
        }

        let expr = &self.ast.exprs[expr_id];

        let value = match &expr.kind {
            ExprKind::Int(i) => ComptimeValue::Int(*i),
            ExprKind::Uint(u) => ComptimeValue::Int(*u as i64),
            ExprKind::Float(f) => ComptimeValue::Float(ComptimeFloat::new(*f)),
            ExprKind::Bool(b) => ComptimeValue::Bool(*b),
            ExprKind::Char(c) => ComptimeValue::Char(*c),
            ExprKind::CStr(s) => ComptimeValue::Cstr(*s),
            ExprKind::Null => ComptimeValue::Option(None),
            ExprKind::Void => ComptimeValue::Void,
            ExprKind::Ident(name) => self.eval_ident(*name)?,
            ExprKind::Group(inner) => self.eval_comptime(*inner)?,
            ExprKind::ArrayLit(elems) => self.eval_array_lit(elems)?,
            ExprKind::ArrayRep { value, count } => self.eval_array_rep(*value, *count)?,
            ExprKind::Block(stmts) => self.eval_block(stmts)?,
            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => self.eval_if(*cond, *then_branch, *else_branch)?,
            ExprKind::While { cond, body } => self.eval_while(*cond, *body)?,
            ExprKind::Loop(body) => self.eval_loop(*body)?,
            ExprKind::Return(value) => {
                let val = self.eval_comptime(*value)?;
                self.context.control_flow = Some(ControlFlow::Return(val.clone()));
                val
            }
            ExprKind::Break(value) => {
                let val = value.map(|v| self.eval_comptime(v)).transpose()?;
                self.context.control_flow = Some(ControlFlow::Break(val.clone()));
                val.unwrap_or(ComptimeValue::Void)
            }
            ExprKind::Continue => {
                self.context.control_flow = Some(ControlFlow::Continue);
                ComptimeValue::Void
            }
            ExprKind::Binary { op, lhs, rhs } => self.eval_binary(*op, *lhs, *rhs)?,
            ExprKind::Unary { op, expr } => self.eval_unary(*op, *expr)?,
            ExprKind::Assign { tgt, val, .. } => self.eval_assignment(*tgt, *val)?,
            ExprKind::Call { callee, args } => self.eval_call(*callee, args)?,
            ExprKind::Index { obj, idx } => self.eval_index(*obj, *idx)?,
            ExprKind::ScopeAccess { object, name } => self.eval_scope_access(*object, *name)?,
            ExprKind::Unwrap(expr) => self.eval_unwrap(*expr)?,
            ExprKind::Comptime(inner) => self.eval_comptime(*inner)?,
            ExprKind::ModuleType { decls } => self.eval_module_type(expr_id, decls)?,
            ExprKind::StructType { fields, decls } => {
                self.eval_struct_type(expr_id, fields, decls)?
            }
            ExprKind::UnionType { fields, decls } => {
                self.eval_union_type(expr_id, fields, decls)?
            }
            ExprKind::EnumType { variants, decls } => {
                self.eval_enum_type(expr_id, variants, decls)?
            }
            ExprKind::TupleType { fields } => self.eval_tuple_type(fields)?,
            ExprKind::ArrayType { elem_ty, len } => self.eval_array_type(*elem_ty, *len)?,
            ExprKind::OptionalType(inner) => self.eval_optional_type(*inner)?,
            ExprKind::PointerType { pointee, mutable } => {
                self.eval_pointer_type(*pointee, *mutable)?
            }
            ExprKind::FnType { params, ret } => self.eval_fn_type(params, *ret)?,
            ExprKind::StructLit { ty: path, fields } => todo!(),
            ExprKind::EnumLit { ty: path, variant } => todo!(),
            ExprKind::TupleLit { fields } => todo!(),
            ExprKind::Cast { expr, ty } => todo!(),
            ExprKind::Match { expr, arms } => todo!(),
            ExprKind::For {
                pattern,
                iter,
                body,
            } => todo!(),
            ExprKind::Field { object, field } => todo!(),
            ExprKind::Range {
                start,
                end,
                inclusive,
            } => todo!(),
            ExprKind::OptionalField { object, field } => todo!(),
        };

        Ok(value)
    }

    fn eval_ident(&mut self, name: Ident) -> ComptimeResult<ComptimeValue> {
        // Find variable with name
        if let Some((value, _)) = self.context.lookup(name) {
            return Ok(value.clone());
        }

        // Find builtin type
        if let Some(&builtin_ty) = self.builtin_types.get(&name) {
            return Ok(ComptimeValue::Type(builtin_ty));
        }

        // Find declaration
        let scope = self.context.current_scope;

        match self.symbol_table.lookup(name, scope) {
            Some((symbol, symbol_scope)) => match &symbol.id {
                SymbolId::Decl(decl_id) => {
                    let prev_scope = mem::replace(&mut self.context.current_scope, symbol_scope);
                    let value = self.eval_decl(*decl_id)?;
                    self.context.current_scope = prev_scope;
                    Ok(value)
                }
                SymbolId::None => Err(ComptimeError::SymbolNotDecl { name, scope }),
            },
            None => Err(ComptimeError::UndefinedSymbol { name, scope }),
        }
    }

    fn eval_decl(&mut self, decl_id: DeclId) -> ComptimeResult<ComptimeValue> {
        if let Some(cached) = self.decl_cache.get(&decl_id) {
            return Ok(cached.clone());
        }

        let value = match &self.ast.decls[decl_id].kind {
            DeclKind::Function { .. } => ComptimeValue::DeclKey { decl_id },
            DeclKind::Const { value, .. } => {
                let prev_decl = self.context.current_decl.replace(decl_id);
                let value = self.eval_comptime(*value)?;
                self.context.current_decl = prev_decl;
                value
            }
        };

        self.decl_cache.insert(decl_id, value.clone());
        Ok(value)
    }

    fn eval_array_lit(&mut self, exprs: &[ExprId]) -> ComptimeResult<ComptimeValue> {
        let mut arr = Vec::with_capacity(exprs.len());
        for expr in exprs {
            arr.push(self.eval_comptime(*expr)?);
        }
        Ok(ComptimeValue::Array(arr.into()))
    }

    fn eval_array_rep(&mut self, value: ExprId, count: ExprId) -> ComptimeResult<ComptimeValue> {
        let value = self.eval_comptime(value)?;

        match self.eval_comptime(count)? {
            ComptimeValue::Int(i) => {
                if i >= 0 {
                    Ok(ComptimeValue::Array(vec![value; i as usize].into()))
                } else {
                    Err(ComptimeError::NegativeArraySize { size: i })
                }
            }
            got => Err(ComptimeError::TypeMismatch { exp: "int", got }),
        }
    }

    fn eval_block(&mut self, stmts: &[StmtId]) -> ComptimeResult<ComptimeValue> {
        self.context.push_scope();

        let mut last_value = ComptimeValue::Void;

        for &stmt_id in stmts {
            if self.context.control_flow.is_some() {
                break;
            }
            last_value = self.eval_stmt(stmt_id)?;
        }

        self.context.pop_scope();

        Ok(last_value)
    }

    fn eval_stmt(&mut self, stmt_id: StmtId) -> ComptimeResult<ComptimeValue> {
        let stmt = self.ast.stmts[stmt_id].clone();

        match &stmt.kind {
            StmtKind::Semi => Ok(ComptimeValue::Void),

            StmtKind::VarDecl { pattern, value, .. } => {
                let val = self.eval_comptime(*value)?;
                let pat = &self.ast.patterns[*pattern];

                match &pat.kind {
                    // TODO: consider other pattern types
                    PatternKind::Variable { mutable, name, .. } => {
                        self.context.bind(*name, val, *mutable);
                        Ok(ComptimeValue::Void)
                    }
                    _ => Err(ComptimeError::UnsupportedPattern { pattern: *pattern }),
                }
            }

            StmtKind::Expr { expr, has_semi } => {
                let val = self.eval_comptime(*expr)?;
                Ok(tnr! {*has_semi => ComptimeValue::Void : val})
            }
        }
    }

    fn eval_if(
        &mut self,
        cond: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    ) -> ComptimeResult<ComptimeValue> {
        match self.eval_comptime(cond)? {
            ComptimeValue::Bool(true) => self.eval_comptime(then_branch),
            ComptimeValue::Bool(false) => {
                if let Some(else_expr) = else_branch {
                    self.eval_comptime(else_expr)
                } else {
                    Ok(ComptimeValue::Void)
                }
            }
            got => Err(ComptimeError::TypeMismatch { exp: "bool", got }),
        }
    }

    fn eval_while(&mut self, cond: ExprId, body: ExprId) -> Result<ComptimeValue, ComptimeError> {
        loop {
            match self.eval_comptime(cond)? {
                ComptimeValue::Bool(true) => {
                    self.eval_comptime(body)?;

                    if let Some(control) = self.context.control_flow.take() {
                        match control {
                            ControlFlow::Break(val) => {
                                return Ok(val.unwrap_or(ComptimeValue::Void));
                            }
                            ControlFlow::Continue => {
                                continue;
                            }
                            ControlFlow::Return(_) => {
                                self.context.control_flow = Some(control);
                                return Ok(ComptimeValue::Void);
                            }
                        }
                    }
                }
                ComptimeValue::Bool(false) => {
                    break;
                }
                got => {
                    return Err(ComptimeError::TypeMismatch { exp: "bool", got });
                }
            }
        }

        Ok(ComptimeValue::Void)
    }

    fn eval_loop(&mut self, body: ExprId) -> Result<ComptimeValue, ComptimeError> {
        loop {
            self.eval_comptime(body)?;

            if let Some(control) = self.context.control_flow.take() {
                match control {
                    ControlFlow::Break(val) => {
                        return Ok(val.unwrap_or(ComptimeValue::Void));
                    }
                    ControlFlow::Continue => {
                        continue;
                    }
                    ControlFlow::Return(_) => {
                        self.context.control_flow = Some(control);
                        return Ok(ComptimeValue::Void);
                    }
                }
            }
        }
    }

    fn eval_binary(
        &mut self,
        op: BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    ) -> ComptimeResult<ComptimeValue> {
        let lhs_val = self.eval_comptime(lhs)?;
        let rhs_val = self.eval_comptime(rhs)?;

        match (op, lhs_val, rhs_val) {
            (BinaryOp::Add, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a + b))
            }
            (BinaryOp::Sub, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a - b))
            }
            (BinaryOp::Mul, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a * b))
            }
            (BinaryOp::Div, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                if b != 0 {
                    Ok(ComptimeValue::Int(a / b))
                } else {
                    Err(ComptimeError::DivisionByZero)
                }
            }
            (BinaryOp::Mod, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                if b != 0 {
                    Ok(ComptimeValue::Int(a % b))
                } else {
                    Err(ComptimeError::DivisionByZero)
                }
            }

            // Integer comparison
            (BinaryOp::Equal, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Bool(a == b))
            }
            (BinaryOp::NotEqual, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Bool(a != b))
            }
            (BinaryOp::Less, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Bool(a < b))
            }
            (BinaryOp::LessEqual, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Bool(a <= b))
            }
            (BinaryOp::Greater, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Bool(a > b))
            }
            (BinaryOp::GreaterEqual, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Bool(a >= b))
            }

            // Boolean logic
            (BinaryOp::And, ComptimeValue::Bool(a), ComptimeValue::Bool(b)) => {
                Ok(ComptimeValue::Bool(a && b))
            }
            (BinaryOp::Or, ComptimeValue::Bool(a), ComptimeValue::Bool(b)) => {
                Ok(ComptimeValue::Bool(a || b))
            }

            // Float arithmetic
            (BinaryOp::Add, ComptimeValue::Float(a), ComptimeValue::Float(b)) => {
                Ok(ComptimeValue::Float(a + b))
            }
            (BinaryOp::Sub, ComptimeValue::Float(a), ComptimeValue::Float(b)) => {
                Ok(ComptimeValue::Float(a - b))
            }
            (BinaryOp::Mul, ComptimeValue::Float(a), ComptimeValue::Float(b)) => {
                Ok(ComptimeValue::Float(a * b))
            }
            (BinaryOp::Div, ComptimeValue::Float(a), ComptimeValue::Float(b)) => {
                Ok(ComptimeValue::Float(a / b))
            }

            // Bitwise operations
            (BinaryOp::BitAnd, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a & b))
            }
            (BinaryOp::BitOr, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a | b))
            }
            (BinaryOp::BitXor, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a ^ b))
            }
            (BinaryOp::Shl, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a << b))
            }
            (BinaryOp::Shr, ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                Ok(ComptimeValue::Int(a >> b))
            }

            (BinaryOp::UnwrapOr, ComptimeValue::Option(opt), default) => match opt {
                Some(val) => Ok(*val),
                None => Ok(default),
            },

            (op, lhs, rhs) => Err(ComptimeError::InvalidBinaryOp { op, lhs, rhs }),
        }
    }

    fn eval_unary(&mut self, op: UnaryOp, expr: ExprId) -> Result<ComptimeValue, ComptimeError> {
        let val = self.eval_comptime(expr)?;

        match (op, val) {
            (UnaryOp::Neg, ComptimeValue::Int(n)) => Ok(ComptimeValue::Int(-n)),
            (UnaryOp::Neg, ComptimeValue::Float(f)) => Ok(ComptimeValue::Float(-f)),
            (UnaryOp::Not, ComptimeValue::Bool(b)) => Ok(ComptimeValue::Bool(!b)),
            (UnaryOp::BitNot, ComptimeValue::Int(n)) => Ok(ComptimeValue::Int(!n)),
            (op, val) => Err(ComptimeError::InvalidUnaryOp { op, val }),
        }
    }

    fn eval_assignment(&mut self, tgt: ExprId, val: ExprId) -> ComptimeResult<ComptimeValue> {
        let val = self.eval_comptime(val)?;
        let target_expr = &self.ast.exprs[tgt];

        if let ExprKind::Ident(name) = target_expr.kind {
            self.context.update(name, val)?;
            Ok(ComptimeValue::Void)
        } else {
            Err(ComptimeError::InvalidAssignmentTarget { tgt })
        }
    }

    fn eval_call(&mut self, callee: ExprId, args: &[ExprId]) -> ComptimeResult<ComptimeValue> {
        let mut call_args = Vec::with_capacity(args.len());
        for &arg in args {
            call_args.push(self.eval_comptime(arg)?);
        }

        let cache_key = (callee, call_args.clone());

        if let Some(cached) = self.generic_cache.get(&cache_key) {
            return Ok(cached.clone());
        }

        let callee = self.eval_comptime(callee)?;

        match callee {
            ComptimeValue::Function { decl_id: item_id } => {
                let result = self.eval_function_call(item_id, &call_args)?;
                self.generic_cache.insert(cache_key, result.clone());
                Ok(result)
            }
            _ => Err(ComptimeError::NotCallable { value: callee }),
        }
    }

    fn eval_function_call(
        &mut self,
        decl_id: DeclId,
        args: &[ComptimeValue],
    ) -> Result<ComptimeValue, ComptimeError> {
        let decl = self.ast.decls[decl_id].clone();

        match &decl.kind {
            DeclKind::Function { params, body, .. } => {
                if params.len() != args.len() {
                    return Err(ComptimeError::ArgumentCountMismatch {
                        exp: params.len(),
                        got: args.len(),
                    });
                }

                self.context.push_scope();

                for (param, arg) in params.iter().zip(args.iter()) {
                    self.context.bind(param.name, arg.clone(), false);
                }

                let result = self.eval_comptime(*body)?;

                let final_result =
                    if let Some(ControlFlow::Return(val)) = self.context.control_flow.take() {
                        val
                    } else {
                        result
                    };

                self.context.pop_scope();

                Ok(final_result)
            }
            _ => Err(ComptimeError::NotAFunction { decl: decl_id }),
        }
    }

    fn eval_index(&mut self, obj: ExprId, idx: ExprId) -> ComptimeResult<ComptimeValue> {
        let arr = match self.eval_comptime(obj)? {
            ComptimeValue::Array(arr) => arr,
            got => return Err(ComptimeError::TypeMismatch { exp: "array", got }),
        };

        match self.eval_comptime(idx)? {
            ComptimeValue::Int(i) => {
                if i >= 0
                    && let Some(v) = arr.get(i as usize)
                {
                    return Ok(v.clone());
                }

                return Err(ComptimeError::IndexOutOfBounds {
                    idx: i,
                    len: arr.len() as i64,
                });
            }
            got => return Err(ComptimeError::TypeMismatch { exp: "int", got }),
        };
    }

    fn eval_scope_access(&mut self, obj: ExprId, name: Ident) -> ComptimeResult<ComptimeValue> {
        let type_id = match self.eval_comptime(obj)? {
            ComptimeValue::Type(t) => t,
            got => return Err(ComptimeError::TypeMismatch { exp: "type", got }),
        };

        let decl = match self.types.get(type_id) {
            TypeValue::Struct(info) => info.find_decl(name),
            TypeValue::Enum(info) => info.find_decl(name),
            TypeValue::Union(info) => info.find_decl(name),
            TypeValue::Module(info) => info.find_decl(name),
            _ => return Err(ComptimeError::NotANamespace { type_id }),
        };

        match decl {
            Some(d) => self.eval_decl(d.id),
            None => Err(ComptimeError::DeclNotFound { type_id, name }),
        }
    }

    fn eval_unwrap(&mut self, expr: ExprId) -> ComptimeResult<ComptimeValue> {
        match self.eval_comptime(expr)? {
            ComptimeValue::Option(Some(inner)) => Ok(*inner),
            ComptimeValue::Option(None) => Err(ComptimeError::UnwrapNone { expr: expr }),
            got => Err(ComptimeError::TypeMismatch {
                exp: "optional",
                got,
            }),
        }
    }

    fn eval_module_type(
        &mut self,
        expr: ExprId,
        decls: &[DeclId],
    ) -> ComptimeResult<ComptimeValue> {
        //TODO: investigate scope_id

        let ty_id = self.types.insert(TypeValue::Module(Default::default()));

        if let Some(decl_id) = self.context.current_decl {
            self.decl_cache.insert(decl_id, ComptimeValue::Type(ty_id));
        }

        let decls = decls
            .iter()
            .map(|&d| Declaration {
                name: self.ast.decls[d].name,
                id: d,
            })
            .collect();

        *self.types.get_mut(ty_id) = TypeValue::Module(ModuleInfo { decls });

        self.type_to_expr.insert(ty_id, expr);

        Ok(ComptimeValue::Type(ty_id))
    }

    fn eval_struct_type(
        &mut self,
        expr: ExprId,
        fields: &[Field],
        decls: &[DeclId],
    ) -> ComptimeResult<ComptimeValue> {
        //TODO: investigate scope_id

        let ty_id = self.types.insert(TypeValue::Union(Default::default()));

        if let Some(decl_id) = self.context.current_decl {
            self.decl_cache.insert(decl_id, ComptimeValue::Type(ty_id));
        }

        let mut struct_fields = Vec::with_capacity(fields.len());
        for field in fields {
            match self.eval_comptime(field.ty)? {
                ComptimeValue::Type(ty) => struct_fields.push(FieldInfo {
                    name: field.name,
                    ty: ty,
                }),
                got => return Err(ComptimeError::TypeMismatch { exp: "type", got }),
            };
        }

        let decls = decls
            .iter()
            .map(|&d| Declaration {
                name: self.ast.decls[d].name,
                id: d,
            })
            .collect();

        *self.types.get_mut(ty_id) = TypeValue::Struct(StructInfo {
            fields: struct_fields,
            decls,
        });

        self.type_to_expr.insert(ty_id, expr);

        Ok(ComptimeValue::Type(ty_id))
    }

    fn eval_union_type(
        &mut self,
        expr: ExprId,
        fields: &[Field],
        decls: &[DeclId],
    ) -> ComptimeResult<ComptimeValue> {
        //TODO: investigate scope_id

        let ty_id = self.types.insert(TypeValue::Void);

        if let Some(decl_id) = self.context.current_decl {
            self.decl_cache.insert(decl_id, ComptimeValue::Type(ty_id));
        }

        let mut union_fields = Vec::with_capacity(fields.len());
        for field in fields {
            match self.eval_comptime(field.ty)? {
                ComptimeValue::Type(ty) => union_fields.push(FieldInfo {
                    name: field.name,
                    ty: ty,
                }),
                got => return Err(ComptimeError::TypeMismatch { exp: "type", got }),
            };
        }

        let decls = decls
            .iter()
            .map(|&d| Declaration {
                name: self.ast.decls[d].name,
                id: d,
            })
            .collect();

        *self.types.get_mut(ty_id) = TypeValue::Union(UnionInfo {
            fields: union_fields,
            decls,
        });

        self.type_to_expr.insert(ty_id, expr);

        Ok(ComptimeValue::Type(ty_id))
    }

    fn eval_enum_type(
        &mut self,
        expr: ExprId,
        variants: &[EnumVariant],
        decls: &[DeclId],
    ) -> ComptimeResult<ComptimeValue> {
        let mut enum_variants = Vec::with_capacity(variants.len());
        let mut current_value = -1;
        let mut seen_values = AHashMap::new();

        for variant in variants {
            current_value = if let Some(v) = variant.value {
                match self.eval_comptime(v)? {
                    ComptimeValue::Int(i) => i,
                    got => return Err(ComptimeError::TypeMismatch { exp: "int", got }),
                }
            } else {
                current_value + 1
            };

            match seen_values.entry(current_value) {
                Entry::Vacant(entry) => {
                    entry.insert(variant.name);
                    enum_variants.push(VariantInfo {
                        name: variant.name,
                        value: current_value,
                    });
                }
                Entry::Occupied(entry) => {
                    return Err(ComptimeError::DuplicateDiscriminant {
                        first: *entry.get(),
                        dupe: variant.name,
                    });
                }
            }
        }

        let decls = decls
            .iter()
            .map(|&d| Declaration {
                name: self.ast.decls[d].name,
                id: d,
            })
            .collect();

        let ty_id = self.types.insert(TypeValue::Enum(EnumInfo {
            variants: enum_variants,
            decls,
        }));

        self.type_to_expr.insert(ty_id, expr);

        Ok(ComptimeValue::Type(ty_id))
    }

    fn eval_tuple_type(&mut self, fields: &[ExprId]) -> ComptimeResult<ComptimeValue> {
        let mut tuple_fields = Vec::with_capacity(fields.len());
        for field in fields {
            match self.eval_comptime(*field)? {
                ComptimeValue::Type(ty) => tuple_fields.push(ty),
                got => {
                    return Err(ComptimeError::TypeMismatch { exp: "type", got });
                }
            };
        }

        let ty_id = self.types.insert(TypeValue::Tuple {
            fields: tuple_fields,
        });

        Ok(ComptimeValue::Type(ty_id))
    }

    fn eval_array_type(&mut self, elem_ty: ExprId, len: ExprId) -> ComptimeResult<ComptimeValue> {
        let element = match self.eval_comptime(elem_ty)? {
            ComptimeValue::Type(ty) => ty,
            got => return Err(ComptimeError::TypeMismatch { exp: "type", got }),
        };

        let len = match self.eval_comptime(len)? {
            ComptimeValue::Int(i) => {
                if i >= 0 {
                    i
                } else {
                    return Err(ComptimeError::NegativeArraySize { size: i });
                }
            }
            got => return Err(ComptimeError::TypeMismatch { exp: "int", got }),
        };

        let ty_id = self.types.insert(TypeValue::Array { element, len });
        Ok(ComptimeValue::Type(ty_id))
    }

    fn eval_optional_type(&mut self, inner: ExprId) -> ComptimeResult<ComptimeValue> {
        match self.eval_comptime(inner)? {
            ComptimeValue::Type(ty) => {
                let ty_id = self.types.insert(TypeValue::Optional(ty));
                Ok(ComptimeValue::Type(ty_id))
            }
            got => Err(ComptimeError::TypeMismatch { exp: "type", got }),
        }
    }

    fn eval_pointer_type(
        &mut self,
        pointee: ExprId,
        mutable: bool,
    ) -> ComptimeResult<ComptimeValue> {
        match self.eval_comptime(pointee)? {
            ComptimeValue::Type(ty) => {
                let ty_id = self.types.insert(TypeValue::Pointer {
                    pointee: ty,
                    mutable,
                });

                Ok(ComptimeValue::Type(ty_id))
            }
            got => Err(ComptimeError::TypeMismatch { exp: "type", got }),
        }
    }

    fn eval_fn_type(
        &mut self,
        params: &[(Option<Ident>, ExprId)],
        ret: ExprId,
    ) -> ComptimeResult<ComptimeValue> {
        let mut func_params = Vec::with_capacity(params.len());

        for &(name, ty) in params {
            match self.eval_comptime(ty)? {
                ComptimeValue::Type(ty) => func_params.push(ParamInfo { name, ty }),
                got => return Err(ComptimeError::TypeMismatch { exp: "type", got }),
            }
        }

        let ret_ty = match self.eval_comptime(ret)? {
            ComptimeValue::Type(ty) => ty,
            got => return Err(ComptimeError::TypeMismatch { exp: "type", got }),
        };

        let ty_id = self.types.insert(TypeValue::Function(FunctionInfo {
            params: func_params,
            ret: ret_ty,
        }));

        Ok(ComptimeValue::Type(ty_id))
    }
}
