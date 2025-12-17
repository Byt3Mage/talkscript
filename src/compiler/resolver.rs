use std::collections::hash_map::Entry;

use ahash::AHashMap;
use simple_ternary::tnr;

use crate::{
    arena::Interner,
    compiler::{
        ast::{
            AstArena, ExprId, ExprKind, Ident, ItemId, ItemKind, PathId, PatternId, PatternKind,
            StmtId, StmtKind, TypeId, TypeKind,
        },
        tokens::Span,
    },
};

pub struct Symbol {
    name: Ident,
    kind: SymbolKind,
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Module,
    Function,
    Variable,
}

pub type ScopeId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Global,
    Module,
    DataType,
    Function,
    Block,
    Loop,
}

pub struct Scope {
    id: ScopeId,
    kind: ScopeKind,
    parent: Option<ScopeId>,
    symbols: AHashMap<Ident, Symbol>,
}

impl Scope {
    pub fn new(id: ScopeId, parent: Option<ScopeId>, kind: ScopeKind) -> Self {
        Self {
            id,
            kind,
            parent,
            symbols: AHashMap::new(),
        }
    }

    pub fn define(&mut self, symbol: Symbol) -> Result<(), (Symbol, Span)> {
        use std::collections::hash_map::Entry;

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
    scopes: Vec<Scope>,
    item_scopes: AHashMap<ItemId, ScopeId>,
    expr_scopes: AHashMap<ExprId, ScopeId>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scopes: vec![],
            item_scopes: AHashMap::new(),
            expr_scopes: AHashMap::new(),
        }
    }

    fn new_item_scope(
        &mut self,
        kind: ScopeKind,
        item_id: ItemId,
        parent: Option<ScopeId>,
    ) -> ScopeId {
        let new_id = self.scopes.len();

        self.scopes.push(Scope::new(new_id, parent, kind));
        self.item_scopes.insert(item_id, new_id);

        new_id
    }

    fn new_expr_scope(
        &mut self,
        kind: ScopeKind,
        expr_id: ExprId,
        parent: Option<ScopeId>,
    ) -> ScopeId {
        let new_id = self.scopes.len();

        self.scopes.push(Scope::new(new_id, parent, kind));
        self.expr_scopes.insert(expr_id, new_id);

        new_id
    }

    /// Look up a symbol in the local scope only
    pub fn lookup_local(&self, name: Ident, scope_id: ScopeId) -> Option<&Symbol> {
        self.scopes[scope_id].symbols.get(&name)
    }

    /// Look up a symbol by name, searching current scope and all parent scopes
    /// Returns the symbol and the scope it was found in
    pub fn lookup(&self, name: Ident, mut scope_id: ScopeId) -> Option<(&Symbol, ScopeId)> {
        loop {
            let scope = &self.scopes[scope_id];

            if let Some(symbol) = scope.symbols.get(&name) {
                return Some((symbol, scope_id));
            }

            scope_id = scope.parent?;
        }
    }

    /// Get mutable reference to a symbol by looking up the scope chain
    pub fn lookup_mut(
        &mut self,
        name: Ident,
        mut scope_id: ScopeId,
    ) -> Option<(&mut Symbol, ScopeId)> {
        let target_scope = loop {
            let scope = &self.scopes[scope_id];

            if scope.symbols.get(&name).is_some() {
                break scope_id;
            }

            scope_id = scope.parent?;
        };

        Some((
            self.scopes[target_scope].symbols.get_mut(&name).unwrap(),
            target_scope,
        ))
    }

    fn define(&mut self, symbol: Symbol, in_scope_id: ScopeId) -> Result<(), (Symbol, Span)> {
        self.scopes[in_scope_id].define(symbol)
    }

    /// Find the nearest enclosing loop scope (for break/continue validation)
    pub fn find_loop_scope(&self, mut scope_id: ScopeId) -> Option<ScopeId> {
        loop {
            let scope = &self.scopes[scope_id];

            if scope.kind == ScopeKind::Loop {
                return Some(scope_id);
            }

            scope_id = scope.parent?;
        }
    }

    /// Find the nearest enclosing function scope
    pub fn find_function_scope(&self, mut scope_id: ScopeId) -> Option<ScopeId> {
        loop {
            let scope = &self.scopes[scope_id];

            if scope.kind == ScopeKind::Function {
                return Some(scope_id);
            }

            scope_id = scope.parent?;
        }
    }
}

struct NameResolver<'a> {
    ast: &'a AstArena,
    interner: &'a mut Interner,
    symbol_table: SymbolTable,
    errors: Vec<ResolveError>,
}

impl<'a> NameResolver<'a> {
    pub fn new(ast: &'a AstArena, interner: &'a mut Interner) -> Self {
        Self {
            ast,
            interner,
            symbol_table: SymbolTable::new(),
            errors: vec![],
        }
    }

    #[inline]
    fn error(&mut self, err: ResolveError) {
        self.errors.push(err);
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn resolve(mut self, root_id: ItemId) -> (SymbolTable, Vec<ResolveError>) {
        // register root
        let root_scope_id = self.symbol_table.scopes.len();
        let root_item = &self.ast.items[root_id];

        self.symbol_table
            .new_item_scope(ScopeKind::Global, root_id, None);

        if let ItemKind::Module { items } = &root_item.kind {
            // First pass: register items
            for item in items {
                self.register_item(*item, root_scope_id);
            }

            for item in items {
                self.resolve_item(*item, root_scope_id);
            }
        }

        (self.symbol_table, self.errors)
    }

    fn register_item(&mut self, item_id: ItemId, in_scope_id: ScopeId) {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Module { items } => {
                // Register module in current scope
                let symbol = Symbol {
                    name: item.name,
                    kind: SymbolKind::Module,
                    span: item.span,
                };

                if let Err((dupe, first_def)) = self.symbol_table.define(symbol, in_scope_id) {
                    self.errors.push(ResolveError::DuplicateSymbol {
                        name: item.name,
                        first_def,
                        dupe_def: dupe.span,
                    });
                }

                // Create module scope and register its items
                let mod_scope_id =
                    self.symbol_table
                        .new_item_scope(ScopeKind::Module, item_id, Some(in_scope_id));

                for &child_item in items {
                    self.register_item(child_item, mod_scope_id);
                }
            }
            ItemKind::Function { .. } => {
                let symbol = Symbol {
                    name: item.name,
                    kind: SymbolKind::Function,
                    span: item.span,
                };

                if let Err((dupe, first_def)) = self.symbol_table.define(symbol, in_scope_id) {
                    self.errors.push(ResolveError::DuplicateSymbol {
                        name: item.name,
                        first_def,
                        dupe_def: dupe.span,
                    });
                }

                self.symbol_table
                    .new_item_scope(ScopeKind::Function, item_id, Some(in_scope_id));
            }
            ItemKind::DataType {
                fields,
                generics,
                methods,
                ..
            } => {
                let kind =
                    tnr! {generics.is_empty() => SymbolKind::Variable : SymbolKind::Function};

                let symbol = Symbol {
                    name: item.name,
                    kind,
                    span: item.span,
                };

                if let Err((dupe, first_def)) = self.symbol_table.define(symbol, in_scope_id) {
                    self.error(ResolveError::DuplicateSymbol {
                        name: item.name,
                        first_def,
                        dupe_def: dupe.span,
                    });
                }

                use std::collections::hash_map::Entry;
                let mut seen = AHashMap::new();
                for field in fields {
                    match seen.entry(field.name) {
                        Entry::Occupied(entry) => self.error(ResolveError::DuplicateField {
                            item_name: item.name,
                            field_name: field.name,
                            first_def: *entry.get(),
                            dupe_def: field.span,
                        }),
                        Entry::Vacant(entry) => {
                            entry.insert(field.span);
                        }
                    }
                }

                let type_scope_id = self.symbol_table.new_item_scope(
                    ScopeKind::DataType,
                    item_id,
                    Some(in_scope_id),
                );

                for &method_id in methods {
                    self.register_item(method_id, type_scope_id);
                }
            }
            ItemKind::Const { generics, .. } => {
                let kind =
                    tnr! {generics.is_empty() => SymbolKind::Variable : SymbolKind::Function};

                let symbol = Symbol {
                    name: item.name,
                    kind,
                    span: item.span,
                };

                if let Err((dupe, first_def)) = self.symbol_table.scopes[in_scope_id].define(symbol)
                {
                    self.error(ResolveError::DuplicateSymbol {
                        name: item.name,
                        first_def,
                        dupe_def: dupe.span,
                    });
                }

                if kind == SymbolKind::Function {
                    self.symbol_table.new_item_scope(
                        ScopeKind::Function,
                        item_id,
                        Some(in_scope_id),
                    );
                }
            }
        }
    }

    fn resolve_item(&mut self, item_id: ItemId, in_scope_id: ScopeId) {
        let item = &self.ast.items[item_id];
        match &item.kind {
            ItemKind::Module { items } => {
                let mod_scope_id = self.symbol_table.item_scopes[&item_id];

                for &child_item in items {
                    self.resolve_item(child_item, mod_scope_id);
                }
            }
            ItemKind::Function {
                generics,
                params,
                ret,
                body,
                ..
            } => {
                let func_scope_id = self.symbol_table.item_scopes[&item_id];

                // Add generics as variables
                for generic in &generics.params {
                    let symbol = Symbol {
                        name: generic.name,
                        kind: SymbolKind::Variable,
                        span: generic.span,
                    };

                    if let Err((dupe, first_def)) = self.symbol_table.define(symbol, func_scope_id)
                    {
                        self.error(ResolveError::DuplicateSymbol {
                            name: generic.name,
                            first_def,
                            dupe_def: dupe.span,
                        });
                    }
                }

                //Resolve generic types (T: type, N: int, etc.)
                for generic in &generics.params {
                    self.resolve_type(generic.ty, func_scope_id);
                }

                for param in params {
                    let symbol = Symbol {
                        name: param.name,
                        kind: SymbolKind::Variable,
                        span: param.span,
                    };

                    if let Err((dupe, first_def)) = self.symbol_table.define(symbol, func_scope_id)
                    {
                        self.error(ResolveError::DuplicateSymbol {
                            name: param.name,
                            first_def,
                            dupe_def: dupe.span,
                        });
                    }
                }

                // Resolve param types (can reference generics)
                for param in params {
                    self.resolve_type(param.ty, func_scope_id);
                }

                // Resolve param types
                self.resolve_type(*ret, func_scope_id);

                // Resolve body
                self.resolve_expr(*body, func_scope_id);
            }
            ItemKind::DataType {
                generics,
                fields,
                methods,
                ..
            } => {
                let type_scope_id = self.symbol_table.item_scopes[&item_id];

                // Add generics as variables
                for generic in &generics.params {
                    let symbol = Symbol {
                        name: generic.name,
                        kind: SymbolKind::Variable,
                        span: generic.span,
                    };

                    if let Err((dupe, first_def)) = self.symbol_table.define(symbol, type_scope_id)
                    {
                        self.error(ResolveError::DuplicateSymbol {
                            name: generic.name,
                            first_def,
                            dupe_def: dupe.span,
                        });
                    }
                }

                // Resolve generic types
                for generic in &generics.params {
                    self.resolve_type(generic.ty, type_scope_id);
                }

                // Resolve field types (can reference generics)
                for field in fields {
                    self.resolve_type(field.ty, type_scope_id);
                }

                // Resolve methods (can reference generics)
                for &method_id in methods {
                    self.resolve_item(method_id, type_scope_id);
                }
            }

            ItemKind::Const {
                generics,
                ty,
                value,
            } => {
                if generics.is_empty() {
                    // Non-generic const - resolve in parent scope
                    if let Some(ty_id) = ty {
                        self.resolve_type(*ty_id, in_scope_id);
                    }
                    self.resolve_expr(*value, in_scope_id);
                } else {
                    let const_scope_id = self.symbol_table.item_scopes[&item_id];

                    // Add generics as variables
                    for generic in &generics.params {
                        let symbol = Symbol {
                            name: generic.name,
                            kind: SymbolKind::Variable,
                            span: generic.span,
                        };

                        if let Err((dupe, first_def)) =
                            self.symbol_table.scopes[const_scope_id].define(symbol)
                        {
                            self.error(ResolveError::DuplicateSymbol {
                                name: generic.name,
                                first_def,
                                dupe_def: dupe.span,
                            });
                        }
                    }

                    // Resolve generic types
                    for generic in &generics.params {
                        self.resolve_type(generic.ty, const_scope_id);
                    }

                    // Resolve type and value with generics in scope
                    if let Some(ty_id) = ty {
                        self.resolve_type(*ty_id, const_scope_id);
                    }
                    self.resolve_expr(*value, const_scope_id);
                };
            }
        }
    }

    fn resolve_type(&mut self, type_id: TypeId, in_scope_id: ScopeId) {
        let ty = &self.ast.types[type_id];

        match &ty.kind {
            TypeKind::Any
            | TypeKind::Int
            | TypeKind::Float
            | TypeKind::Bool
            | TypeKind::String
            | TypeKind::Char
            | TypeKind::Void
            | TypeKind::Never
            | TypeKind::Type
            | TypeKind::Infer => {}

            TypeKind::Path(path_id) => self.resolve_path(*path_id, in_scope_id),

            TypeKind::Optional(inner) => self.resolve_type(*inner, in_scope_id),

            TypeKind::DynArray(element) => self.resolve_type(*element, in_scope_id),

            // Array - resolve element type and length expression
            TypeKind::Array { element, len } => {
                self.resolve_type(*element, in_scope_id);
                self.resolve_expr(*len, in_scope_id);
            }

            TypeKind::Pointer { pointee, .. } => self.resolve_type(*pointee, in_scope_id),

            TypeKind::Function { params, ret } => {
                for param_ty in params {
                    self.resolve_type(*param_ty, in_scope_id);
                }
                self.resolve_type(*ret, in_scope_id);
            }

            TypeKind::AnonStruct { fields, methods } => {
                let struct_name = self.interner.get_or_intern_static("<anonymous>");

                let mut seen = AHashMap::new();
                for field in fields {
                    match seen.entry(field.name) {
                        Entry::Occupied(entry) => self.error(ResolveError::DuplicateField {
                            item_name: struct_name,
                            field_name: field.name,
                            first_def: *entry.get(),
                            dupe_def: field.span,
                        }),
                        Entry::Vacant(entry) => {
                            entry.insert(field.span);
                        }
                    }
                }

                for field in fields {
                    self.resolve_type(field.ty, in_scope_id);
                }

                for &method_id in methods {
                    self.resolve_item(method_id, in_scope_id);
                }
            }

            TypeKind::Tuple { fields } => {
                for field_ty in fields {
                    self.resolve_type(*field_ty, in_scope_id);
                }
            }

            TypeKind::AnonEnum { variants, methods } => {
                let struct_name = self.interner.get_or_intern_static("<anonymous>");

                let mut seen = AHashMap::new();

                for variant in variants {
                    match seen.entry(variant.name) {
                        Entry::Occupied(entry) => self.error(ResolveError::DuplicateField {
                            item_name: struct_name,
                            field_name: variant.name,
                            first_def: *entry.get(),
                            dupe_def: variant.span,
                        }),
                        Entry::Vacant(entry) => {
                            entry.insert(variant.span);
                        }
                    }
                }

                for variant in variants {
                    self.resolve_type(variant.ty, in_scope_id);
                }

                for &method_id in methods {
                    self.resolve_item(method_id, in_scope_id);
                }
            }

            // Range - resolve element type
            TypeKind::Range { element, .. } => {
                self.resolve_type(*element, in_scope_id);
            }
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId, in_scope_id: ScopeId) {
        let expr = &self.ast.exprs[expr_id];

        match &expr.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::CStr(_)
            | ExprKind::Char(_)
            | ExprKind::Null
            | ExprKind::Void => {}

            ExprKind::TypeLit(ty) => {
                self.resolve_type(*ty, in_scope_id);
            }

            ExprKind::Ident(name) => {
                if let Some((symbol, _)) = self.symbol_table.lookup(*name, in_scope_id) {
                    // Check if it's a valid symbol for expression context
                    match symbol.kind {
                        SymbolKind::Variable | SymbolKind::Function => {
                            // Valid in expression
                        }
                        SymbolKind::Module => {
                            self.error(ResolveError::InvalidSymbolInExpression {
                                name: *name,
                                span: expr.span,
                                kind: symbol.kind,
                            });
                        }
                    }
                } else {
                    self.error(ResolveError::UndefinedSymbol {
                        name: *name,
                        use_span: expr.span,
                    });
                }
            }

            ExprKind::Path(path_id) => {
                self.resolve_path(*path_id, in_scope_id);
            }

            ExprKind::ArrayLit(elements) => {
                for &elem in elements {
                    self.resolve_expr(elem, in_scope_id);
                }
            }

            ExprKind::ArrayRepeat { value, count } => {
                self.resolve_expr(*value, in_scope_id);
                self.resolve_expr(*count, in_scope_id);
            }

            ExprKind::StructLit { path, fields } => {
                if let Some(path_id) = path {
                    self.resolve_path(*path_id, in_scope_id);
                }

                for field in fields {
                    if let Some(val) = field.value {
                        self.resolve_expr(val, in_scope_id);
                    }
                }
            }

            ExprKind::EnumLit { path, field } => {
                if let Some(path_id) = path {
                    self.resolve_path(*path_id, in_scope_id);
                }

                if let Some(value_expr) = field.value {
                    self.resolve_expr(value_expr, in_scope_id);
                }
            }

            ExprKind::TupleLit { fields } => {
                for field in fields {
                    self.resolve_expr(field, in_scope_id);
                }
            }

            ExprKind::Group(inner) => {
                self.resolve_expr(*inner, in_scope_id);
            }

            ExprKind::Unary { expr, .. } => {
                self.resolve_expr(*expr, in_scope_id);
            }

            ExprKind::Binary { lhs, rhs, .. } => {
                self.resolve_expr(*lhs, in_scope_id);
                self.resolve_expr(*rhs, in_scope_id);
            }

            ExprKind::Assign { target, value, .. } => {
                self.resolve_expr(*target, in_scope_id);
                self.resolve_expr(*value, in_scope_id);
            }

            ExprKind::Range { start, end, .. } => {
                if let Some(start_expr) = start {
                    self.resolve_expr(*start_expr, in_scope_id);
                }
                if let Some(end_expr) = end {
                    self.resolve_expr(*end_expr, in_scope_id);
                }
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

            ExprKind::Block(stmts) => {
                // Create block scope
                let block_scope_id =
                    self.symbol_table
                        .new_expr_scope(ScopeKind::Block, expr_id, Some(in_scope_id));

                // Resolve statements in block scope
                for &stmt_id in stmts {
                    self.resolve_stmt(stmt_id, block_scope_id);
                }
            }

            ExprKind::While { cond, body } => {
                self.resolve_expr(*cond, in_scope_id);

                // Create loop scope for body
                let loop_scope_id =
                    self.symbol_table
                        .new_expr_scope(ScopeKind::Loop, expr_id, Some(in_scope_id));

                self.resolve_expr(*body, loop_scope_id);
            }

            ExprKind::Loop(body) => {
                // Create loop scope
                let loop_scope_id =
                    self.symbol_table
                        .new_expr_scope(ScopeKind::Loop, expr_id, Some(in_scope_id));

                self.resolve_expr(*body, loop_scope_id);
            }

            ExprKind::Match { expr, arms } => {
                self.resolve_expr(*expr, in_scope_id);

                for arm in arms {
                    // Resolve pattern
                    self.resolve_pattern(arm.pattern, in_scope_id);

                    // Resolve guard if present
                    if let Some(guard_expr) = arm.guard {
                        self.resolve_expr(guard_expr, in_scope_id);
                    }

                    // Resolve body
                    self.resolve_expr(arm.body, in_scope_id);
                }
            }

            ExprKind::Return(value) => {
                self.resolve_expr(*value, in_scope_id);

                // Check we're inside a function
                if self.symbol_table.find_function_scope(in_scope_id).is_none() {
                    self.error(ResolveError::ReturnOutsideFunction { span: expr.span });
                }
            }

            ExprKind::Break(value) => {
                self.resolve_expr(*value, in_scope_id);

                // Check we're inside a loop
                if self.symbol_table.find_loop_scope(in_scope_id).is_none() {
                    self.error(ResolveError::BreakOutsideLoop { span: expr.span });
                }
            }

            ExprKind::Continue => {
                // Check we're inside a loop
                if self.symbol_table.find_loop_scope(in_scope_id).is_none() {
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

            ExprKind::Index { object, index } => {
                self.resolve_expr(*object, in_scope_id);
                self.resolve_expr(*index, in_scope_id);
            }

            ExprKind::Cast { expr, ty } => {
                self.resolve_expr(*expr, in_scope_id);
                self.resolve_type(*ty, in_scope_id);
            }

            ExprKind::Comptime(inner) => {
                self.resolve_expr(*inner, in_scope_id);
            }
        }
    }

    fn resolve_path(&mut self, path_id: PathId, in_scope_id: ScopeId) {
        let path = &self.ast.paths[path_id];

        // For now, handle simple single-segment paths (just an identifier)
        // TODO: Handle multi-segment paths like std::vec::Vec

        if path.segments.len() == 1 {
            let ident = path.segments[0].ident;

            // Look up in symbol table
            if self.symbol_table.lookup(ident, in_scope_id).is_none() {
                self.error(ResolveError::UndefinedSymbol {
                    name: ident,
                    use_span: path.span,
                });
            }
        } else {
            // Multi-segment path - TODO
            todo!("resolve multi-segment paths");
        }
    }

    fn resolve_stmt(&mut self, stmt_id: StmtId, in_scope_id: ScopeId) {
        let stmt = &self.ast.stmts[stmt_id];

        match &stmt.kind {
            // Empty statement
            StmtKind::Semi => {}

            // Variable declaration
            StmtKind::VarDecl {
                mutable,
                pattern,
                ty,
                value,
            } => {
                // Resolve type annotation if present
                if let Some(ty_id) = ty {
                    self.resolve_type(*ty_id, in_scope_id);
                }

                // Resolve value expression
                self.resolve_expr(*value, in_scope_id);

                // Add bindings from pattern to scope
                self.add_pattern_bindings(*pattern, in_scope_id);
            }

            // Comptime variable declaration
            StmtKind::ConstVarDecl {
                pattern, ty, value, ..
            } => {
                if let Some(ty_id) = ty {
                    self.resolve_type(*ty_id, in_scope_id);
                }

                // Resolve value expression
                self.resolve_expr(*value, in_scope_id);

                // Add bindings from pattern to scope
                self.add_pattern_bindings(*pattern, in_scope_id);
            }

            // Expression statement
            StmtKind::Expr { expr, .. } => {
                self.resolve_expr(*expr, in_scope_id);
            }
        }
    }
    fn resolve_pattern(&mut self, pattern_id: PatternId, in_scope_id: ScopeId) {
        let pattern = &self.ast.patterns[pattern_id];

        match &pattern.kind {
            // Wildcard - nothing to resolve
            PatternKind::Wildcard => {}

            // Variable binding - nothing to resolve (just a name)
            PatternKind::Variable { .. } => {}

            // Path pattern - resolve the path
            PatternKind::Path(path_id) => {
                self.resolve_path(*path_id, in_scope_id);
            }

            // Struct destructuring
            PatternKind::Struct { path, fields } => {
                if let Some(path_id) = path {
                    self.resolve_path(*path_id, in_scope_id);
                }

                // Resolve nested patterns
                for field in fields {
                    self.resolve_pattern(field.pattern, in_scope_id);
                }
            }

            // Tuple destructuring
            PatternKind::Tuple { elements } => {
                for &elem in elements {
                    self.resolve_pattern(elem, in_scope_id);
                }
            }

            // Enum destructuring
            PatternKind::Enum { path, pattern } => {
                if let Some(path_id) = path {
                    self.resolve_path(*path_id, in_scope_id);
                }

                if let Some(pat_id) = pattern {
                    self.resolve_pattern(*pat_id, in_scope_id);
                }
            }

            // Literal patterns
            PatternKind::Lit(expr_id) => {
                self.resolve_expr(*expr_id, in_scope_id);
            }

            // Or pattern
            PatternKind::Or(patterns) => {
                for &pat in patterns {
                    self.resolve_pattern(pat, in_scope_id);
                }
            }
        }
    }

    fn add_pattern_bindings(&mut self, pattern_id: PatternId, in_scope_id: ScopeId) {
        let pattern = &self.ast.patterns[pattern_id];

        match &pattern.kind {
            // Wildcard - no binding
            PatternKind::Wildcard => {}

            // Variable binding - add to scope
            PatternKind::Variable { name, .. } => {
                let symbol = Symbol {
                    name: *name,
                    kind: SymbolKind::Variable,
                    span: pattern.span,
                };

                if let Err((dupe, first_def)) = self.symbol_table.scopes[in_scope_id].define(symbol)
                {
                    self.error(ResolveError::DuplicateSymbol {
                        name: *name,
                        first_def,
                        dupe_def: dupe.span,
                    });
                }
            }

            // Path - no new bindings
            PatternKind::Path(_) => {}

            // Struct - recursively add bindings from fields
            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.add_pattern_bindings(field.pattern, in_scope_id);
                }
            }

            // Tuple - recursively add bindings from elements
            PatternKind::Tuple { elements } => {
                for &elem in elements {
                    self.add_pattern_bindings(elem, in_scope_id);
                }
            }

            // Enum - recursively add bindings from nested pattern
            PatternKind::Enum { pattern, .. } => {
                if let Some(pat_id) = pattern {
                    self.add_pattern_bindings(*pat_id, in_scope_id);
                }
            }

            // Literal - no bindings
            PatternKind::Lit(_) => {}

            // Or - add bindings from all alternatives (they must be the same)
            PatternKind::Or(patterns) => {
                // For now, just add from first pattern
                // Type checker should verify all alternatives bind same names
                if let Some(&first) = patterns.first() {
                    self.add_pattern_bindings(first, in_scope_id);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResolveError {
    /// Symbol is defined multiple times in the same scope
    DuplicateSymbol {
        name: Ident,
        first_def: Span,
        dupe_def: Span,
    },
    DuplicateField {
        item_name: Ident,
        field_name: Ident,
        first_def: Span,
        dupe_def: Span,
    },
    /// Symbol is not found in any scope
    UndefinedSymbol {
        name: Ident,
        use_span: Span,
    },

    /// Variable used before initialization
    UninitVariable {
        name: Ident,
        use_span: Span,
        def_span: Span,
    },

    /// break used outside of a loop
    BreakOutsideLoop {
        span: Span,
    },

    /// continue used outside of a loop
    ContinueOutsideLoop {
        span: Span,
    },

    /// return used outside of a function
    ReturnOutsideFunction {
        span: Span,
    },

    InvalidSymbolInExpression {
        name: Ident,
        span: Span,
        kind: SymbolKind,
    },

    TypeNotFound {
        name: Ident,
        use_span: Span,
    },
    Internal {
        msg: String,
        span: Span,
    },
}
