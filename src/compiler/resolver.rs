use std::collections::hash_map::Entry;

use ahash::AHashMap;

use crate::{
    arena::{Ident, Interner},
    compiler::{
        canon_ast::{
            CanonAstArena, DeclId, DeclKind, ExprId, ExprKind, PatternId, PatternKind, StmtId,
            StmtKind,
        },
        tokens::Span,
    },
};

#[derive(Debug, Clone)]
pub enum ResolveError {
    DuplicateSymbol {
        name: Ident,
        first_def: Span,
        dupe_def: Span,
    },
    UndefinedSymbol {
        name: Ident,
        use_span: Span,
    },
    BreakOutsideLoop {
        span: Span,
    },
    ContinueOutsideLoop {
        span: Span,
    },
    ReturnOutsideFunction {
        span: Span,
    },
    RootNotModule,
}

pub enum SymbolId {
    None,
    Decl(DeclId),
}
pub struct Symbol {
    pub id: SymbolId,
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeId {
    Decl(DeclId),
    Expr(ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Global,
    Type,
    Function,
    Block,
    Loop,
}

pub struct Scope {
    kind: ScopeKind,
    parent: Option<ScopeId>,
    symbols: AHashMap<Ident, Symbol>,
}

impl Scope {
    pub fn new(kind: ScopeKind, parent: Option<ScopeId>) -> Self {
        Self {
            kind,
            parent,
            symbols: AHashMap::new(),
        }
    }

    pub fn define(&mut self, symbol: Symbol) -> Result<(), (Symbol, Span)> {
        match self.symbols.entry(symbol.name) {
            Entry::Occupied(entry) => Err((symbol, entry.get().span)),
            Entry::Vacant(entry) => {
                entry.insert(symbol);
                Ok(())
            }
        }
    }
}

pub struct SymbolTable {
    pub scopes: AHashMap<ScopeId, Scope>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scopes: AHashMap::new(),
        }
    }

    fn new_scope(&mut self, id: ScopeId, kind: ScopeKind, parent: Option<ScopeId>) {
        self.scopes.insert(id, Scope::new(kind, parent));
    }

    pub fn lookup_local(&self, name: Ident, scope_id: ScopeId) -> Option<&Symbol> {
        self.scopes[&scope_id].symbols.get(&name)
    }

    pub fn lookup(&self, name: Ident, mut scope_id: ScopeId) -> Option<(&Symbol, ScopeId)> {
        loop {
            let scope = &self.scopes[&scope_id];
            if let Some(symbol) = scope.symbols.get(&name) {
                return Some((symbol, scope_id));
            }
            scope_id = scope.parent?;
        }
    }

    fn define(&mut self, symbol: Symbol, in_scope_id: ScopeId) -> Result<(), (Symbol, Span)> {
        self.scopes
            .get_mut(&in_scope_id)
            .expect("scope not found")
            .define(symbol)
    }

    pub fn find_loop_scope(&self, mut scope_id: ScopeId) -> Option<ScopeId> {
        loop {
            let scope = &self.scopes[&scope_id];
            if scope.kind == ScopeKind::Loop {
                return Some(scope_id);
            }
            scope_id = scope.parent?;
        }
    }

    pub fn find_function_scope(&self, mut scope_id: ScopeId) -> Option<ScopeId> {
        loop {
            let scope = &self.scopes[&scope_id];
            if scope.kind == ScopeKind::Function {
                return Some(scope_id);
            }
            scope_id = scope.parent?;
        }
    }
}

struct NameResolver<'a> {
    ast: &'a CanonAstArena,
    interner: &'a mut Interner,
    table: SymbolTable,
    errors: Vec<ResolveError>,
}

impl<'a> NameResolver<'a> {
    pub fn new(ast: &'a CanonAstArena, interner: &'a mut Interner) -> Self {
        Self {
            ast,
            interner,
            table: SymbolTable::new(),
            errors: vec![],
        }
    }

    #[inline]
    fn error(&mut self, err: ResolveError) {
        self.errors.push(err);
    }

    pub fn resolve(mut self, root_decl: DeclId) -> (SymbolTable, Vec<ResolveError>) {
        let scope_id = ScopeId::Decl(root_decl);

        self.table.new_scope(scope_id, ScopeKind::Global, None);

        if let DeclKind::Const { value, .. } = &self.ast.decls[root_decl].kind {
            if let ExprKind::ModuleType { decls } = &self.ast.exprs[*value].kind {
                // First pass: register all declarations
                for &decl_id in decls {
                    self.register_decl(decl_id, scope_id);
                }

                // Second pass: resolve all declarations
                for &decl_id in decls {
                    self.resolve_decl(decl_id, scope_id);
                }

                return (self.table, self.errors);
            }
        }

        self.error(ResolveError::RootNotModule);
        (self.table, self.errors)
    }

    fn register_decl(&mut self, decl_id: DeclId, in_scope_id: ScopeId) {
        let decl = &self.ast.decls[decl_id];

        let symbol = Symbol {
            id: SymbolId::Decl(decl_id),
            name: decl.name,
            span: decl.span,
        };

        if let Err((dupe, first_def)) = self.table.define(symbol, in_scope_id) {
            self.error(ResolveError::DuplicateSymbol {
                name: decl.name,
                first_def,
                dupe_def: dupe.span,
            });
        }

        match &decl.kind {
            DeclKind::Const { .. } => {}
            DeclKind::Function { .. } => {
                self.table.new_scope(
                    ScopeId::Decl(decl_id),
                    ScopeKind::Function,
                    Some(in_scope_id),
                );
            }
        }
    }

    fn resolve_decl(&mut self, decl_id: DeclId, in_scope_id: ScopeId) {
        let decl = &self.ast.decls[decl_id];

        match &decl.kind {
            DeclKind::Const { ty, value } => {
                if let Some(ty_expr) = ty {
                    self.resolve_expr(*ty_expr, in_scope_id);
                }
                self.resolve_expr(*value, in_scope_id);
            }

            DeclKind::Function {
                params, ret, body, ..
            } => {
                let func_scope_id = ScopeId::Decl(decl_id);

                // Add comptime parameters first
                for param in params {
                    if param.is_comptime {
                        let symbol = Symbol {
                            id: SymbolId::None,
                            name: param.name,
                            span: param.span,
                        };

                        if let Err((dupe, first_def)) = self.table.define(symbol, func_scope_id) {
                            self.error(ResolveError::DuplicateSymbol {
                                name: param.name,
                                first_def,
                                dupe_def: dupe.span,
                            });
                        }

                        self.resolve_expr(param.ty, func_scope_id);
                    }
                }

                // Add regular parameters
                for param in params {
                    if !param.is_comptime {
                        let symbol = Symbol {
                            id: SymbolId::None,
                            name: param.name,
                            span: param.span,
                        };

                        if let Err((dupe, first_def)) = self.table.define(symbol, func_scope_id) {
                            self.error(ResolveError::DuplicateSymbol {
                                name: param.name,
                                first_def,
                                dupe_def: dupe.span,
                            });
                        }

                        self.resolve_expr(param.ty, func_scope_id);
                    }
                }

                self.resolve_expr(*ret, func_scope_id);
                self.resolve_expr(*body, func_scope_id);
            }
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId, in_scope_id: ScopeId) {
        let expr = &self.ast.exprs[expr_id];

        match &expr.kind {
            ExprKind::Int(_)
            | ExprKind::Uint(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::CStr(_)
            | ExprKind::Char(_)
            | ExprKind::Null
            | ExprKind::Void => {}

            ExprKind::Ident(name) => {
                if self.table.lookup(*name, in_scope_id).is_none() {
                    self.error(ResolveError::UndefinedSymbol {
                        name: *name,
                        use_span: expr.span,
                    });
                }
            }

            ExprKind::Group(inner) => {
                self.resolve_expr(*inner, in_scope_id);
            }

            ExprKind::ArrayLit(elements) => {
                for &elem in elements {
                    self.resolve_expr(elem, in_scope_id);
                }
            }

            ExprKind::ArrayRep { value, count } => {
                self.resolve_expr(*value, in_scope_id);
                self.resolve_expr(*count, in_scope_id);
            }

            ExprKind::StructLit { ty: path, fields } => {
                if let Some(path_expr) = path {
                    self.resolve_expr(*path_expr, in_scope_id);
                }

                for field in fields {
                    if let Some(val) = field.value {
                        self.resolve_expr(val, in_scope_id);
                    }
                }
            }

            ExprKind::EnumLit { ty: path, .. } => {
                if let Some(path_expr) = path {
                    self.resolve_expr(*path_expr, in_scope_id);
                }
            }

            ExprKind::UnionLit { ty: path, field } => {
                if let Some(path_expr) = path {
                    self.resolve_expr(*path_expr, in_scope_id);
                }

                if let Some(val) = field.value {
                    self.resolve_expr(val, in_scope_id);
                }
            }

            ExprKind::TupleLit { fields } => {
                for field in fields {
                    self.resolve_expr(*field, in_scope_id);
                }
            }

            ExprKind::Unary { expr, .. } => {
                self.resolve_expr(*expr, in_scope_id);
            }

            ExprKind::Binary { lhs, rhs, .. } => {
                self.resolve_expr(*lhs, in_scope_id);
                self.resolve_expr(*rhs, in_scope_id);
            }

            ExprKind::Assign { tgt, val, .. } => {
                self.resolve_expr(*tgt, in_scope_id);
                self.resolve_expr(*val, in_scope_id);
            }

            ExprKind::Cast { expr, ty } => {
                self.resolve_expr(*expr, in_scope_id);
                self.resolve_expr(*ty, in_scope_id);
            }

            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(*cond, in_scope_id);
                self.resolve_expr(*then_branch, in_scope_id);
                if let Some(else_expr) = else_branch {
                    self.resolve_expr(*else_expr, in_scope_id);
                }
            }

            ExprKind::Match { expr, arms } => {
                self.resolve_expr(*expr, in_scope_id);

                for arm in arms {
                    self.resolve_pattern(arm.pattern, in_scope_id);

                    if let Some(guard_expr) = arm.guard {
                        self.resolve_expr(guard_expr, in_scope_id);
                    }

                    self.resolve_expr(arm.body, in_scope_id);
                }
            }

            ExprKind::While { cond, body } => {
                self.resolve_expr(*cond, in_scope_id);

                let loop_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(loop_scope_id, ScopeKind::Loop, Some(in_scope_id));

                self.resolve_expr(*body, loop_scope_id);
            }

            ExprKind::Loop(body) => {
                let loop_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(loop_scope_id, ScopeKind::Loop, Some(in_scope_id));

                self.resolve_expr(*body, loop_scope_id);
            }

            ExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.resolve_expr(*iter, in_scope_id);

                let loop_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(loop_scope_id, ScopeKind::Loop, Some(in_scope_id));

                self.resolve_pattern(*pattern, loop_scope_id);
                self.add_pattern_bindings(*pattern, loop_scope_id);

                self.resolve_expr(*body, loop_scope_id);
            }

            ExprKind::Block(stmts) => {
                let block_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(block_scope_id, ScopeKind::Loop, Some(in_scope_id));

                for &stmt_id in stmts {
                    self.resolve_stmt(stmt_id, block_scope_id);
                }
            }

            ExprKind::Return(value) => {
                self.resolve_expr(*value, in_scope_id);

                if self.table.find_function_scope(in_scope_id).is_none() {
                    self.error(ResolveError::ReturnOutsideFunction { span: expr.span });
                }
            }

            ExprKind::Break(value) => {
                if let Some(val_expr) = value {
                    self.resolve_expr(*val_expr, in_scope_id);
                }

                if self.table.find_loop_scope(in_scope_id).is_none() {
                    self.error(ResolveError::BreakOutsideLoop { span: expr.span });
                }
            }

            ExprKind::Continue => {
                if self.table.find_loop_scope(in_scope_id).is_none() {
                    self.error(ResolveError::ContinueOutsideLoop { span: expr.span });
                }
            }

            ExprKind::Call { callee, args } => {
                self.resolve_expr(*callee, in_scope_id);

                for &arg in args {
                    self.resolve_expr(arg, in_scope_id);
                }
            }

            ExprKind::Field { object, .. } => {
                self.resolve_expr(*object, in_scope_id);
            }

            ExprKind::ScopeAccess { object, .. } => {
                self.resolve_expr(*object, in_scope_id);
            }

            ExprKind::OptionalField { object, .. } => {
                self.resolve_expr(*object, in_scope_id);
            }

            ExprKind::Index { obj, idx } => {
                self.resolve_expr(*obj, in_scope_id);
                self.resolve_expr(*idx, in_scope_id);
            }

            ExprKind::Range { start, end, .. } => {
                if let Some(start_expr) = start {
                    self.resolve_expr(*start_expr, in_scope_id);
                }
                if let Some(end_expr) = end {
                    self.resolve_expr(*end_expr, in_scope_id);
                }
            }

            ExprKind::Unwrap(inner) => {
                self.resolve_expr(*inner, in_scope_id);
            }

            ExprKind::Comptime(inner) => {
                self.resolve_expr(*inner, in_scope_id);
            }

            ExprKind::StructType { fields, decls } => {
                let struct_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(struct_scope_id, ScopeKind::Type, Some(in_scope_id));

                for field in fields {
                    self.resolve_expr(field.ty, in_scope_id);
                }

                for &decl_id in decls {
                    self.register_decl(decl_id, struct_scope_id);
                }
                for &decl_id in decls {
                    self.resolve_decl(decl_id, struct_scope_id);
                }
            }

            ExprKind::UnionType { fields, decls } => {
                let union_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(union_scope_id, ScopeKind::Type, Some(in_scope_id));

                for field in fields {
                    self.resolve_expr(field.ty, in_scope_id);
                }

                for &decl_id in decls {
                    self.register_decl(decl_id, union_scope_id);
                }
                for &decl_id in decls {
                    self.resolve_decl(decl_id, union_scope_id);
                }
            }

            ExprKind::EnumType { variants, decls } => {
                let enum_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(enum_scope_id, ScopeKind::Type, Some(in_scope_id));

                for variant in variants {
                    if let Some(value_expr) = variant.value {
                        self.resolve_expr(value_expr, in_scope_id);
                    }
                }

                for &decl_id in decls {
                    self.register_decl(decl_id, enum_scope_id);
                }

                for &decl_id in decls {
                    self.resolve_decl(decl_id, enum_scope_id);
                }
            }

            ExprKind::TupleType { fields } => {
                for field in fields {
                    self.resolve_expr(*field, in_scope_id);
                }
            }

            ExprKind::ArrayType { elem_ty, len } => {
                self.resolve_expr(*elem_ty, in_scope_id);
                self.resolve_expr(*len, in_scope_id);
            }

            ExprKind::PointerType { pointee, .. } => {
                self.resolve_expr(*pointee, in_scope_id);
            }

            ExprKind::OptionalType(inner) => {
                self.resolve_expr(*inner, in_scope_id);
            }

            ExprKind::ModuleType { decls } => {
                let mod_scope_id = ScopeId::Expr(expr_id);

                self.table
                    .new_scope(mod_scope_id, ScopeKind::Type, Some(in_scope_id));

                for &decl_id in decls {
                    self.register_decl(decl_id, mod_scope_id);
                }

                for &decl_id in decls {
                    self.resolve_decl(decl_id, mod_scope_id);
                }
            }

            ExprKind::FnType { params, ret } => {
                for (_, param_ty) in params {
                    self.resolve_expr(*param_ty, in_scope_id);
                }

                self.resolve_expr(*ret, in_scope_id);
            }
        }
    }

    fn resolve_stmt(&mut self, stmt_id: StmtId, in_scope_id: ScopeId) {
        let stmt = &self.ast.stmts[stmt_id];

        match &stmt.kind {
            StmtKind::Semi => {}

            StmtKind::VarDecl {
                pattern, ty, value, ..
            } => {
                if let Some(ty_expr) = ty {
                    self.resolve_expr(*ty_expr, in_scope_id);
                }

                self.resolve_expr(*value, in_scope_id);

                self.add_pattern_bindings(*pattern, in_scope_id);
            }

            StmtKind::Expr { expr, .. } => {
                self.resolve_expr(*expr, in_scope_id);
            }
        }
    }

    fn resolve_pattern(&mut self, pattern_id: PatternId, in_scope_id: ScopeId) {
        let pattern = &self.ast.patterns[pattern_id];

        match &pattern.kind {
            PatternKind::Wildcard | PatternKind::Variable { .. } => {}

            PatternKind::Int(_)
            | PatternKind::Float(_)
            | PatternKind::Bool(_)
            | PatternKind::Char(_)
            | PatternKind::String(_) => {}

            PatternKind::Struct { path, fields } => {
                if let Some(path_expr) = path {
                    self.resolve_expr(*path_expr, in_scope_id);
                }

                for field in fields {
                    self.resolve_pattern(field.pattern, in_scope_id);
                }
            }

            PatternKind::Tuple { elements } => {
                for &elem in elements {
                    self.resolve_pattern(elem, in_scope_id);
                }
            }

            PatternKind::Enum { path, .. } => {
                if let Some(path_expr) = path {
                    self.resolve_expr(*path_expr, in_scope_id);
                }
            }

            PatternKind::Lit(expr_id) => {
                self.resolve_expr(*expr_id, in_scope_id);
            }

            PatternKind::Or(patterns) => {
                for &pat in patterns {
                    self.resolve_pattern(pat, in_scope_id);
                }
            }

            PatternKind::Range { start, end, .. } => {
                if let Some(start_expr) = start {
                    self.resolve_expr(*start_expr, in_scope_id);
                }
                if let Some(end_expr) = end {
                    self.resolve_expr(*end_expr, in_scope_id);
                }
            }
        }
    }

    fn add_pattern_bindings(&mut self, pattern_id: PatternId, in_scope_id: ScopeId) {
        let pattern = &self.ast.patterns[pattern_id];

        match &pattern.kind {
            PatternKind::Wildcard => {}

            PatternKind::Variable { name, .. } => {
                let symbol = Symbol {
                    id: SymbolId::None,
                    name: *name,
                    span: pattern.span,
                };

                if let Err((dupe, first_def)) = self.table.define(symbol, in_scope_id) {
                    self.error(ResolveError::DuplicateSymbol {
                        name: *name,
                        first_def,
                        dupe_def: dupe.span,
                    });
                }
            }

            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.add_pattern_bindings(field.pattern, in_scope_id);
                }
            }

            PatternKind::Tuple { elements } => {
                for &elem in elements {
                    self.add_pattern_bindings(elem, in_scope_id);
                }
            }

            PatternKind::Or(patterns) => {
                if let Some(&first) = patterns.first() {
                    self.add_pattern_bindings(first, in_scope_id);
                }
            }

            _ => {}
        }
    }
}
