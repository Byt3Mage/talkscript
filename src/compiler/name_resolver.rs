use std::collections::hash_map::Entry;

use ahash::AHashMap;

use crate::{
    arena::{Ident, Interner},
    compiler::{
        ast::{
            AstArena, AstTypeId, AstTypeKind, ExprId, ExprKind, Field, GenericArg,
            GenericParamKind, GenericParams, ItemId, ItemKind, Path, PatternId, PatternKind,
            StmtId, StmtKind, Variant,
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
    DuplicateField {
        name: Ident,
        first_def: Span,
        dupe_def: Span,
    },
    DuplicateVariant {
        name: Ident,
        first_def: Span,
        dupe_def: Span,
    },
    UndefinedSymbol {
        name: Ident,
        span: Span,
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
    SelfOutsideImpl {
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Item(ItemId),
    Variable,     // let bindings, function params
    GenericType,  // <T>
    GenericConst, // <const N: int>
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeId {
    Item(ItemId),
    Expr(ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Module,
    Function,
    Impl,
    Type,
    Block,
    Loop,
}

pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub symbols: AHashMap<Ident, Symbol>,
}

impl Scope {
    pub fn new(kind: ScopeKind, parent: Option<ScopeId>) -> Self {
        Self {
            kind,
            parent,
            symbols: AHashMap::new(),
        }
    }

    pub fn define(&mut self, symbol: Symbol) -> Result<(), Span> {
        match self.symbols.entry(symbol.name) {
            Entry::Occupied(entry) => Err(entry.get().span),
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
        self.scopes.get(&scope_id)?.symbols.get(&name)
    }

    pub fn lookup(&self, name: Ident, mut scope_id: ScopeId) -> Option<(&Symbol, ScopeId)> {
        loop {
            let scope = self.scopes.get(&scope_id)?;
            if let Some(symbol) = scope.symbols.get(&name) {
                return Some((symbol, scope_id));
            }
            scope_id = scope.parent?;
        }
    }

    fn define(&mut self, symbol: Symbol, scope_id: ScopeId) -> Result<(), Span> {
        self.scopes
            .get_mut(&scope_id)
            .expect("scope not found")
            .define(symbol)
    }

    pub fn find_enclosing(&self, mut scope_id: ScopeId, kind: ScopeKind) -> Option<ScopeId> {
        loop {
            let scope = self.scopes.get(&scope_id)?;
            if scope.kind == kind {
                return Some(scope_id);
            }
            scope_id = scope.parent?;
        }
    }

    pub fn find_loop_scope(&self, scope_id: ScopeId) -> Option<ScopeId> {
        self.find_enclosing(scope_id, ScopeKind::Loop)
    }

    pub fn find_function_scope(&self, scope_id: ScopeId) -> Option<ScopeId> {
        self.find_enclosing(scope_id, ScopeKind::Function)
    }

    pub fn find_impl_scope(&self, scope_id: ScopeId) -> Option<ScopeId> {
        self.find_enclosing(scope_id, ScopeKind::Impl)
    }
}

pub struct NameResolver<'a> {
    ast: &'a AstArena,
    interner: &'a Interner,
    table: SymbolTable,
    errors: Vec<ResolveError>,
}

impl<'a> NameResolver<'a> {
    pub fn new(ast: &'a AstArena, interner: &'a Interner) -> Self {
        Self {
            ast,
            interner,
            table: SymbolTable::new(),
            errors: vec![],
        }
    }

    pub fn resolve(mut self, root: ItemId) -> (SymbolTable, Vec<ResolveError>) {
        let scope_id = ScopeId::Item(root);
        self.table.new_scope(scope_id, ScopeKind::Module, None);

        if let ItemKind::Module { items } = &self.ast.items[root].kind {
            // First pass: register all items
            for &item_id in items {
                self.register_item(item_id, scope_id);
            }

            // Second pass: resolve all items
            for &item_id in items {
                self.resolve_item(item_id, scope_id);
            }
        }

        (self.table, self.errors)
    }

    fn error(&mut self, err: ResolveError) {
        self.errors.push(err);
    }

    fn define(&mut self, symbol: Symbol, scope_id: ScopeId) {
        let name = symbol.name;
        let span = symbol.span;
        if let Err(first_def) = self.table.define(symbol, scope_id) {
            self.error(ResolveError::DuplicateSymbol {
                name,
                first_def,
                dupe_def: span,
            });
        }
    }

    fn register_item(&mut self, item_id: ItemId, scope_id: ScopeId) {
        let item = &self.ast.items[item_id];

        self.define(
            Symbol {
                kind: SymbolKind::Item(item_id),
                name: item.name,
                span: item.span,
            },
            scope_id,
        );

        // Create inner scopes for items that need them
        match &item.kind {
            ItemKind::Module { items } => {
                let mod_scope = ScopeId::Item(item_id);
                self.table
                    .new_scope(mod_scope, ScopeKind::Module, Some(scope_id));

                for &inner_id in items {
                    self.register_item(inner_id, mod_scope);
                }
            }

            ItemKind::Function { .. } => {
                self.table
                    .new_scope(ScopeId::Item(item_id), ScopeKind::Function, Some(scope_id));
            }

            ItemKind::Struct { .. }
            | ItemKind::Enum { .. }
            | ItemKind::Union { .. }
            | ItemKind::Const { .. }
            | ItemKind::TypeAlias { .. }
            | ItemKind::Import(_) => {}
        }
    }

    fn resolve_item(&mut self, item_id: ItemId, parent_scope: ScopeId) {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Module { items } => {
                let mod_scope = ScopeId::Item(item_id);
                for &inner_id in items {
                    self.resolve_item(inner_id, mod_scope);
                }
            }

            ItemKind::Function {
                generics,
                params,
                ret,
                body,
            } => {
                let func_scope = ScopeId::Item(item_id);

                // Register generic parameters
                self.resolve_generics(generics, func_scope);

                // Register function parameters
                for param in params {
                    self.resolve_type(param.ty, func_scope);
                    self.add_pattern_bindings(param.pattern, func_scope);
                }

                // Resolve return type
                if let Some(ret_ty) = ret {
                    self.resolve_type(*ret_ty, func_scope);
                }

                // Resolve body
                self.resolve_expr(*body, func_scope);
            }

            ItemKind::Struct { generics, fields } => {
                let struct_scope = ScopeId::Item(item_id);
                self.table
                    .new_scope(struct_scope, ScopeKind::Type, Some(parent_scope));
                self.resolve_generics(generics, struct_scope);
                self.resolve_fields(fields, struct_scope);
            }

            ItemKind::Enum { variants } => {
                self.resolve_variants(variants, parent_scope);
            }

            ItemKind::Union { generics, fields } => {
                let union_scope = ScopeId::Item(item_id);
                self.table
                    .new_scope(union_scope, ScopeKind::Type, Some(parent_scope));
                self.resolve_generics(generics, union_scope);
                self.resolve_fields(fields, union_scope);
            }

            ItemKind::Const { ty, value } => {
                if let Some(ty) = ty {
                    self.resolve_type(*ty, parent_scope);
                }
                self.resolve_expr(*value, parent_scope);
            }

            ItemKind::TypeAlias { generics, ty } => {
                let alias_scope = ScopeId::Item(item_id);
                self.table
                    .new_scope(alias_scope, ScopeKind::Type, Some(parent_scope));
                self.resolve_generics(generics, alias_scope);
                self.resolve_type(*ty, alias_scope);
            }

            ItemKind::Import(_) => {
                // Import resolution is typically a separate phase
            }
        }
    }

    fn resolve_generics(&mut self, generics: &GenericParams, scope_id: ScopeId) {
        for param in &generics.params {
            let kind = match &param.kind {
                GenericParamKind::Type { default } => {
                    if let Some(default_ty) = default {
                        self.resolve_type(*default_ty, scope_id);
                    }
                    SymbolKind::GenericType
                }
                GenericParamKind::Const { ty, default } => {
                    self.resolve_type(*ty, scope_id);
                    if let Some(default_expr) = default {
                        self.resolve_expr(*default_expr, scope_id);
                    }
                    SymbolKind::GenericConst
                }
            };

            self.define(
                Symbol {
                    kind,
                    name: param.name,
                    span: param.span,
                },
                scope_id,
            );
        }
    }

    // ========================================================================
    // Types
    // ========================================================================

    fn resolve_type(&mut self, type_id: AstTypeId, scope_id: ScopeId) {
        let ty = &self.ast.types[type_id];

        match &ty.kind {
            AstTypeKind::Path(path) => {
                self.resolve_path(path, scope_id);
            }

            AstTypeKind::Pointer { pointee, .. } => {
                self.resolve_type(*pointee, scope_id);
            }

            AstTypeKind::Optional(inner) => {
                self.resolve_type(*inner, scope_id);
            }

            AstTypeKind::Array { elem, size } => {
                self.resolve_type(*elem, scope_id);
                self.resolve_expr(*size, scope_id);
            }

            AstTypeKind::Slice(elem) => {
                self.resolve_type(*elem, scope_id);
            }

            AstTypeKind::Tuple(fields) => {
                for &field in fields {
                    self.resolve_type(field, scope_id);
                }
            }

            AstTypeKind::Function { params, ret } => {
                for &param in params {
                    self.resolve_type(param, scope_id);
                }
                self.resolve_type(*ret, scope_id);
            }

            AstTypeKind::SelfType => {
                if self.table.find_impl_scope(scope_id).is_none() {
                    self.error(ResolveError::SelfOutsideImpl { span: ty.span });
                }
            }

            AstTypeKind::Infer => {}

            AstTypeKind::AnonStruct(fields) => {
                for field in fields {
                    self.resolve_type(field.ty, scope_id);
                }
            }

            AstTypeKind::AnonUnion(fields) => {
                for field in fields {
                    self.resolve_type(field.ty, scope_id);
                }
            }

            AstTypeKind::AnonEnum(variants) => {
                for variant in variants {
                    if let Some(value) = variant.value {
                        self.resolve_expr(value, scope_id);
                    }
                }
            }
        }
    }

    // ========================================================================
    // Expressions
    // ========================================================================

    fn resolve_expr(&mut self, expr_id: ExprId, scope_id: ScopeId) {
        let expr = &self.ast.exprs[expr_id];

        match &expr.kind {
            ExprKind::Int(_)
            | ExprKind::Uint(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Char(_)
            | ExprKind::Cstr(_)
            | ExprKind::Void => {}

            ExprKind::Path(path) => {
                self.resolve_path(path, scope_id);
            }

            ExprKind::Group(inner) => {
                self.resolve_expr(*inner, scope_id);
            }

            ExprKind::ArrayLit(elems) => {
                for &elem in elems {
                    self.resolve_expr(elem, scope_id);
                }
            }

            ExprKind::ArrayRepeat { value, count } => {
                self.resolve_expr(*value, scope_id);
                self.resolve_expr(*count, scope_id);
            }

            ExprKind::StructLit { path, fields } => {
                self.resolve_path(path, scope_id);
                for field in fields {
                    if let Some(value) = field.value {
                        self.resolve_expr(value, scope_id);
                    }
                }
            }

            ExprKind::AnonStructLit { fields } => {
                for field in fields {
                    if let Some(value) = field.value {
                        self.resolve_expr(value, scope_id);
                    }
                }
            }

            ExprKind::UnionLit { path, field } => {
                self.resolve_path(path, scope_id);
                if let Some(value) = field.value {
                    self.resolve_expr(value, scope_id);
                }
            }

            ExprKind::AnonUnionLit { field } => {
                if let Some(value) = field.value {
                    self.resolve_expr(value, scope_id);
                }
            }

            ExprKind::TupleLit(elems) => {
                for &elem in elems {
                    self.resolve_expr(elem, scope_id);
                }
            }

            ExprKind::Unary { expr, .. } => {
                self.resolve_expr(*expr, scope_id);
            }

            ExprKind::Binary { lhs, rhs, .. } => {
                self.resolve_expr(*lhs, scope_id);
                self.resolve_expr(*rhs, scope_id);
            }

            ExprKind::Assign { target, value, .. } => {
                self.resolve_expr(*target, scope_id);
                self.resolve_expr(*value, scope_id);
            }

            ExprKind::Cast { expr, ty } => {
                self.resolve_expr(*expr, scope_id);
                self.resolve_type(*ty, scope_id);
            }

            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(*cond, scope_id);
                self.resolve_expr(*then_branch, scope_id);
                if let Some(else_br) = else_branch {
                    self.resolve_expr(*else_br, scope_id);
                }
            }

            ExprKind::Match { scrutinee, arms } => {
                self.resolve_expr(*scrutinee, scope_id);
                for arm in arms {
                    // Each arm gets its own scope for pattern bindings
                    let arm_scope = ScopeId::Expr(expr_id);
                    self.table
                        .new_scope(arm_scope, ScopeKind::Block, Some(scope_id));

                    self.resolve_pattern(arm.pattern, arm_scope);
                    self.add_pattern_bindings(arm.pattern, arm_scope);

                    if let Some(guard) = arm.guard {
                        self.resolve_expr(guard, arm_scope);
                    }
                    self.resolve_expr(arm.body, arm_scope);
                }
            }

            ExprKind::While { cond, body } => {
                self.resolve_expr(*cond, scope_id);

                let loop_scope = ScopeId::Expr(expr_id);
                self.table
                    .new_scope(loop_scope, ScopeKind::Loop, Some(scope_id));
                self.resolve_expr(*body, loop_scope);
            }

            ExprKind::Loop(body) => {
                let loop_scope = ScopeId::Expr(expr_id);
                self.table
                    .new_scope(loop_scope, ScopeKind::Loop, Some(scope_id));
                self.resolve_expr(*body, loop_scope);
            }

            ExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.resolve_expr(*iter, scope_id);

                let loop_scope = ScopeId::Expr(expr_id);
                self.table
                    .new_scope(loop_scope, ScopeKind::Loop, Some(scope_id));

                self.resolve_pattern(*pattern, loop_scope);
                self.add_pattern_bindings(*pattern, loop_scope);
                self.resolve_expr(*body, loop_scope);
            }

            ExprKind::Block(stmts) => {
                let block_scope = ScopeId::Expr(expr_id);
                self.table
                    .new_scope(block_scope, ScopeKind::Block, Some(scope_id));

                for &stmt_id in stmts {
                    self.resolve_stmt(stmt_id, block_scope);
                }
            }

            ExprKind::Return(value) => {
                if let Some(val) = value {
                    self.resolve_expr(*val, scope_id);
                }
                if self.table.find_function_scope(scope_id).is_none() {
                    self.error(ResolveError::ReturnOutsideFunction { span: expr.span });
                }
            }

            ExprKind::Break(value) => {
                if let Some(val) = value {
                    self.resolve_expr(*val, scope_id);
                }
                if self.table.find_loop_scope(scope_id).is_none() {
                    self.error(ResolveError::BreakOutsideLoop { span: expr.span });
                }
            }

            ExprKind::Continue => {
                if self.table.find_loop_scope(scope_id).is_none() {
                    self.error(ResolveError::ContinueOutsideLoop { span: expr.span });
                }
            }

            ExprKind::Call { callee, args } => {
                self.resolve_expr(*callee, scope_id);
                for &arg in args {
                    self.resolve_expr(arg, scope_id);
                }
            }

            ExprKind::MethodCall {
                receiver,
                type_args,
                args,
                ..
            } => {
                self.resolve_expr(*receiver, scope_id);
                for &ty_arg in type_args {
                    self.resolve_type(ty_arg, scope_id);
                }
                for &arg in args {
                    self.resolve_expr(arg, scope_id);
                }
            }

            ExprKind::Field { object, .. } => {
                self.resolve_expr(*object, scope_id);
            }

            ExprKind::OptionalChain { object, .. } => {
                self.resolve_expr(*object, scope_id);
            }

            ExprKind::Index { object, index } => {
                self.resolve_expr(*object, scope_id);
                self.resolve_expr(*index, scope_id);
            }

            ExprKind::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.resolve_expr(*s, scope_id);
                }
                if let Some(e) = end {
                    self.resolve_expr(*e, scope_id);
                }
            }

            ExprKind::Unwrap(inner) => {
                self.resolve_expr(*inner, scope_id);
            }

            ExprKind::Comptime(inner) => {
                self.resolve_expr(*inner, scope_id);
            }
        }
    }

    // ========================================================================
    // Statements
    // ========================================================================

    fn resolve_stmt(&mut self, stmt_id: StmtId, scope_id: ScopeId) {
        let stmt = &self.ast.stmts[stmt_id];

        match &stmt.kind {
            StmtKind::Let { pattern, ty, value } => {
                if let Some(ty) = ty {
                    self.resolve_type(*ty, scope_id);
                }
                self.resolve_expr(*value, scope_id);
                self.add_pattern_bindings(*pattern, scope_id);
            }

            StmtKind::Expr { expr, .. } => {
                self.resolve_expr(*expr, scope_id);
            }

            StmtKind::Empty => {}
        }
    }

    // ========================================================================
    // Patterns
    // ========================================================================

    fn resolve_pattern(&mut self, pattern_id: PatternId, scope_id: ScopeId) {
        let pattern = &self.ast.patterns[pattern_id];

        match &pattern.kind {
            PatternKind::Wildcard | PatternKind::Binding { .. } => {}

            PatternKind::Int(_)
            | PatternKind::Float(_)
            | PatternKind::Bool(_)
            | PatternKind::Char(_)
            | PatternKind::CStr(_) => {}

            PatternKind::Path(path) => {
                self.resolve_path(path, scope_id);
            }

            PatternKind::Struct { path, fields, .. } => {
                self.resolve_path(path, scope_id);
                for field in fields {
                    self.resolve_pattern(field.pattern, scope_id);
                }
            }

            PatternKind::AnonStruct { fields, .. } => {
                for field in fields {
                    self.resolve_pattern(field.pattern, scope_id);
                }
            }

            PatternKind::Union { path, field } => {
                self.resolve_path(path, scope_id);
                self.resolve_pattern(field.pattern, scope_id);
            }

            PatternKind::AnonUnion { field } => {
                self.resolve_pattern(field.pattern, scope_id);
            }

            PatternKind::Tuple(elems) => {
                for &elem in elems {
                    self.resolve_pattern(elem, scope_id);
                }
            }

            PatternKind::Array(elems) => {
                for &elem in elems {
                    self.resolve_pattern(elem, scope_id);
                }
            }

            PatternKind::Or(patterns) => {
                for &pat in patterns {
                    self.resolve_pattern(pat, scope_id);
                }
            }

            PatternKind::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.resolve_expr(*s, scope_id);
                }
                if let Some(e) = end {
                    self.resolve_expr(*e, scope_id);
                }
            }
        }
    }

    fn add_pattern_bindings(&mut self, pattern_id: PatternId, scope_id: ScopeId) {
        let pattern = &self.ast.patterns[pattern_id];

        match &pattern.kind {
            PatternKind::Wildcard
            | PatternKind::Path(_)
            | PatternKind::Int(_)
            | PatternKind::Float(_)
            | PatternKind::Bool(_)
            | PatternKind::Char(_)
            | PatternKind::CStr(_)
            | PatternKind::Range { .. } => {}

            PatternKind::Binding { name, .. } => {
                self.define(
                    Symbol {
                        kind: SymbolKind::Variable,
                        name: *name,
                        span: pattern.span,
                    },
                    scope_id,
                );
            }

            PatternKind::Struct { fields, .. } | PatternKind::AnonStruct { fields, .. } => {
                for field in fields {
                    self.add_pattern_bindings(field.pattern, scope_id);
                }
            }

            PatternKind::Union { field, .. } | PatternKind::AnonUnion { field } => {
                self.add_pattern_bindings(field.pattern, scope_id);
            }

            PatternKind::Tuple(elems) | PatternKind::Array(elems) => {
                for &elem in elems {
                    self.add_pattern_bindings(elem, scope_id);
                }
            }

            PatternKind::Or(patterns) => {
                // All branches must bind the same names - just use first
                if let Some(&first) = patterns.first() {
                    self.add_pattern_bindings(first, scope_id);
                }
            }
        }
    }

    // ========================================================================
    // Paths
    // ========================================================================

    fn resolve_path(&mut self, path: &Path, scope_id: ScopeId) {
        // Resolve first segment as a name lookup
        if let Some(first) = path.segments.first() {
            if self.table.lookup(first.name, scope_id).is_none() {
                self.error(ResolveError::UndefinedSymbol {
                    name: first.name,
                    span: first.span,
                });
            }

            // Resolve type arguments in all segments
            for segment in &path.segments {
                for ty_arg in &segment.args {
                    match ty_arg {
                        GenericArg::Type(ty) => self.resolve_type(*ty, scope_id),
                        GenericArg::Const(expr) => self.resolve_expr(*expr, scope_id),
                    }
                }
            }
        }

        // Note: subsequent segments (a::b::c) are resolved during type checking,
        // not name resolution, because we need to know what 'a' is first
    }

    fn resolve_fields(&mut self, fields: &[Field], scope_id: ScopeId) {
        let mut seen: AHashMap<Ident, Span> = AHashMap::new();
        for field in fields {
            match seen.entry(field.name) {
                Entry::Occupied(entry) => {
                    self.error(ResolveError::DuplicateField {
                        name: field.name,
                        first_def: *entry.get(),
                        dupe_def: field.span,
                    });
                }
                Entry::Vacant(entry) => {
                    entry.insert(field.span);
                }
            }
            self.resolve_type(field.ty, scope_id);
        }
    }

    fn resolve_variants(&mut self, variants: &[Variant], scope_id: ScopeId) {
        let mut seen: AHashMap<Ident, Span> = AHashMap::new();
        for variant in variants {
            match seen.entry(variant.name) {
                Entry::Occupied(entry) => {
                    self.error(ResolveError::DuplicateField {
                        name: variant.name,
                        first_def: *entry.get(),
                        dupe_def: variant.span,
                    });
                }
                Entry::Vacant(entry) => {
                    entry.insert(variant.span);
                }
            }

            if let Some(value) = variant.value {
                self.resolve_expr(value, scope_id);
            }
        }
    }
}

pub fn resolve(
    ast: &AstArena,
    interner: &Interner,
    root: ItemId,
) -> (SymbolTable, Vec<ResolveError>) {
    NameResolver::new(ast, interner).resolve(root)
}
