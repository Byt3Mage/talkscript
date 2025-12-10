use ahash::AHashMap;

use crate::{
    arena::Interner,
    compiler::{
        ast::{
            AstArena, ExprId, ExprKind, Ident, ItemId, ItemKind, StmtId, StmtKind, TypeId, TypeKind,
        },
        tokens::Span,
    },
};

/// Identifies what kind of symbol this is
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Module { item_id: ItemId },
    Function { item_id: ItemId },
    Variable { mutable: bool, ty: Option<TypeId> },
    Type,
}

/// Information about a symbol in a scope
#[derive(Debug, Clone)]
pub struct Symbol {
    /// The identifier name
    pub name: Ident,
    /// What kind of symbol this is
    pub kind: SymbolKind,
    /// Where this symbol was defined
    pub def_span: Span,
}

/// A unique identifier for scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Top-level/package scope
    Global,
    /// Module scope
    Module,
    /// Function body
    Function,
    /// Block expression
    Block,
    /// Loop body (for break/continue context)
    Loop,
}

/// Represents a lexical scope
#[derive(Debug)]
pub struct Scope {
    /// Unique identifier for this scope
    pub id: ScopeId,
    /// Parent scope (None for global scope)
    pub parent: Option<ScopeId>,
    /// Symbols defined directly in this scope
    symbols: AHashMap<Ident, Symbol>,
    /// Kind of scope (for context)
    pub kind: ScopeKind,
}

impl Scope {
    pub fn new(id: ScopeId, parent: Option<ScopeId>, kind: ScopeKind) -> Self {
        Self {
            id,
            parent,
            symbols: AHashMap::new(),
            kind,
        }
    }

    /// Insert a symbol into this scope
    /// Returns the old symbol if one existed (for shadowing detection)
    pub fn insert(&mut self, symbol: Symbol) -> Result<(), (Symbol, Span)> {
        use std::collections::hash_map::Entry;

        match self.symbols.entry(symbol.name) {
            Entry::Occupied(entry) => Err((symbol, entry.get().def_span)),
            Entry::Vacant(entry) => {
                entry.insert(symbol);
                Ok(())
            }
        }
    }

    /// Look up a symbol in this scope only (does not check parent scopes)
    pub fn get(&self, name: Ident) -> Option<&Symbol> {
        self.symbols.get(&name)
    }

    /// Get a mutable reference to a symbol in this scope
    pub fn get_mut(&mut self, name: Ident) -> Option<&mut Symbol> {
        self.symbols.get_mut(&name)
    }

    /// Iterate over all symbols in this scope
    pub fn iter(&self) -> impl Iterator<Item = (&Ident, &Symbol)> {
        self.symbols.iter()
    }
}

/// Manages all scopes and symbol resolution
pub struct SymbolTable {
    /// All scopes indexed by ScopeId
    scopes: Vec<Scope>,
    item_scopes: AHashMap<ItemId, ScopeId>,
    expr_scopes: AHashMap<ExprId, ScopeId>,
}

impl SymbolTable {
    /// Create a new symbol table with a global scope
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            item_scopes: AHashMap::new(),
            expr_scopes: AHashMap::new(),
        }
    }

    pub fn new_item_scope(&mut self, kind: ScopeKind, item_id: ItemId, parent: ScopeId) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope::new(id, Some(parent), kind));
        self.item_scopes.insert(item_id, id);
        id
    }

    pub fn new_expr_scope(&mut self, kind: ScopeKind, expr_id: ExprId, parent: ScopeId) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope::new(id, Some(parent), kind));
        self.expr_scopes.insert(expr_id, id);
        id
    }

    pub fn get_scope(&self, scope_id: ScopeId) -> &Scope {
        &self.scopes[scope_id.0]
    }

    pub fn get_scope_mut(&mut self, scope_id: ScopeId) -> &mut Scope {
        &mut self.scopes[scope_id.0]
    }

    /// Look up a symbol by name, searching current scope and all parent scopes
    /// Returns the symbol and the scope it was found in
    pub fn lookup(&self, name: Ident, mut scope_id: ScopeId) -> Option<(&Symbol, ScopeId)> {
        loop {
            let scope = &self.scopes[scope_id.0];

            if let Some(symbol) = scope.get(name) {
                return Some((symbol, scope_id));
            }

            scope_id = scope.parent?;
        }
    }

    /// Look up a symbol in the local scope only
    pub fn lookup_local(&self, name: Ident, scope_id: ScopeId) -> Option<&Symbol> {
        self.scopes[scope_id.0].get(name)
    }

    /// Get mutable reference to a symbol by looking up the scope chain
    pub fn lookup_mut(
        &mut self,
        name: Ident,
        mut scope_id: ScopeId,
    ) -> Option<(&mut Symbol, ScopeId)> {
        let target_scope = loop {
            let scope = &self.scopes[scope_id.0];

            if scope.get(name).is_some() {
                break scope_id;
            }

            scope_id = scope.parent?;
        };

        Some((
            self.scopes[target_scope.0].get_mut(name).unwrap(),
            target_scope,
        ))
    }

    /// Find the nearest enclosing loop scope (for break/continue validation)
    pub fn find_loop_scope(&self, mut scope_id: ScopeId) -> Option<ScopeId> {
        loop {
            let scope = &self.scopes[scope_id.0];

            if scope.kind == ScopeKind::Loop {
                return Some(scope_id);
            }

            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return None,
            }
        }
    }

    /// Find the nearest enclosing function scope
    pub fn find_function_scope(&self, mut scope_id: ScopeId) -> Option<ScopeId> {
        loop {
            let scope = &self.scopes[scope_id.0];

            if scope.kind == ScopeKind::Function {
                return Some(scope_id);
            }

            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return None,
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
        struct_name: Ident,
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

pub struct NameResolver<'a> {
    ast: &'a AstArena,
    interner: &'a Interner,
    symbol_table: SymbolTable,
    errors: Vec<ResolveError>,
}

impl<'a> NameResolver<'a> {
    pub fn new(ast: &'a AstArena, interner: &'a Interner) -> Self {
        Self {
            ast,
            interner,
            symbol_table: SymbolTable::new(),
            errors: vec![],
        }
    }

    pub fn resolve(mut self, root_id: ItemId) -> (SymbolTable, Vec<ResolveError>) {
        debug_assert!(self.symbol_table.scopes.len() == 0);

        let root_scope_id = ScopeId(0);
        let root_item = &self.ast.items[root_id];

        self.symbol_table.scopes.push(Scope {
            id: root_scope_id,
            parent: None,
            symbols: AHashMap::new(),
            kind: ScopeKind::Global,
        });

        self.symbol_table.item_scopes.insert(root_id, root_scope_id);

        match &root_item.kind {
            ItemKind::Module { items } => {
                // First pass: register items
                for item in items {
                    self.register_item(*item, root_scope_id);
                }

                // Second pass: resolve names
                for item in items {
                    self.resolve_item(*item, root_scope_id);
                }
            }
            _ => self.errors.push(ResolveError::Internal {
                msg: "expected module item as root".to_string(),
                span: root_item.span,
            }),
        }

        (self.symbol_table, self.errors)
    }

    /// Register item in the given scope
    fn register_item(&mut self, item_id: ItemId, in_scope: ScopeId) {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Module { items } => {
                let scope = self.symbol_table.get_scope_mut(in_scope);

                let symbol = Symbol {
                    name: item.name,
                    kind: SymbolKind::Module { item_id },
                    def_span: item.span,
                };

                if let Err((dupe, first_def)) = scope.insert(symbol) {
                    self.errors.push(ResolveError::DuplicateSymbol {
                        name: item.name,
                        first_def,
                        dupe_def: dupe.def_span,
                    });
                }

                // Create module scope and register its items
                let mod_scope =
                    self.symbol_table
                        .new_item_scope(ScopeKind::Module, item_id, in_scope);

                for &child_id in items {
                    self.register_item(child_id, mod_scope);
                }
            }

            ItemKind::Function { .. } => {
                let scope = self.symbol_table.get_scope_mut(in_scope);

                let symbol = Symbol {
                    name: item.name,
                    kind: SymbolKind::Function { item_id },
                    def_span: item.span,
                };

                if let Err((dupe, first_def)) = scope.insert(symbol) {
                    self.errors.push(ResolveError::DuplicateSymbol {
                        name: item.name,
                        first_def,
                        dupe_def: dupe.def_span,
                    });
                }
            }

            ItemKind::Struct { fields } => {
                use std::collections::hash_map::Entry;

                // Validate field names
                let mut seen_fields = AHashMap::new();
                for field in fields {
                    match seen_fields.entry(field.name) {
                        Entry::Vacant(entry) => {
                            entry.insert(field.span);
                        }
                        Entry::Occupied(entry) => {
                            self.errors.push(ResolveError::DuplicateField {
                                struct_name: item.name,
                                field_name: field.name,
                                first_def: *entry.get(),
                                dupe_def: field.span,
                            });
                        }
                    }
                }

                let scope = self.symbol_table.get_scope_mut(in_scope);

                let symbol = Symbol {
                    name: item.name,
                    kind: SymbolKind::Type,
                    def_span: item.span,
                };

                if let Err((dupe, first_def)) = scope.insert(symbol) {
                    self.errors.push(ResolveError::DuplicateSymbol {
                        name: item.name,
                        first_def,
                        dupe_def: dupe.def_span,
                    });
                }
            }
        }
    }

    fn resolve_item(&mut self, item_id: ItemId, in_scope: ScopeId) {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Module { items } => {
                let mod_scope = self.symbol_table.item_scopes[&item_id];
                for &child_item in items {
                    self.resolve_item(child_item, mod_scope);
                }
            }
            ItemKind::Function { params, body, .. } => {
                let func_scope = self.symbol_table.item_scopes[&item_id];

                for param in params {
                    self.resolve_type(param.ty, in_scope);

                    let scope = self.symbol_table.get_scope_mut(func_scope);

                    let symbol = Symbol {
                        name: param.name,
                        kind: SymbolKind::Variable {
                            ty: Some(param.ty),
                            mutable: param.mutable,
                        },
                        def_span: param.span,
                    };

                    if let Err((dupe, first_def)) = scope.insert(symbol) {
                        self.errors.push(ResolveError::DuplicateSymbol {
                            name: param.name,
                            first_def,
                            dupe_def: dupe.def_span,
                        });
                    }
                }

                self.resolve_expr(*body, func_scope);
            }
            ItemKind::Struct { fields } => {
                for field in fields {
                    self.resolve_type(field.ty, in_scope);
                }
            }
        }
    }

    fn resolve_stmt(&mut self, stmt_id: StmtId, in_scope: ScopeId) {
        let stmt = &self.ast.stmts[stmt_id];

        match &stmt.kind {
            StmtKind::Semi => {}
            StmtKind::VarDecl {
                mutable, name, ty, ..
            } => {
                if let Some(type_id) = ty {
                    self.resolve_type(*type_id, in_scope);
                }

                let symbol = Symbol {
                    name: *name,
                    kind: SymbolKind::Variable {
                        ty: *ty,
                        mutable: *mutable,
                    },
                    def_span: stmt.span,
                };

                let scope = self.symbol_table.get_scope_mut(in_scope);

                if let Err((dupe, first_def)) = scope.insert(symbol) {
                    self.errors.push(ResolveError::DuplicateSymbol {
                        name: *name,
                        first_def,
                        dupe_def: dupe.def_span,
                    });
                }
            }
            StmtKind::Expr { expr, .. } => self.resolve_expr(*expr, in_scope),
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId, in_scope: ScopeId) {
        let expr = &self.ast.exprs[expr_id];

        match &expr.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::String(_)
            | ExprKind::Null
            | ExprKind::Void
            | ExprKind::Continue => {}

            ExprKind::ArrayLit(elems) => elems.iter().for_each(|e| self.resolve_expr(*e, in_scope)),
            ExprKind::Ident(name) => match self.symbol_table.lookup(*name, in_scope) {
                Some((symbol, _)) => match symbol.kind {
                    SymbolKind::Variable { .. } | SymbolKind::Function { .. } => {}
                    _ => self.errors.push(ResolveError::InvalidSymbolInExpression {
                        name: *name,
                        span: expr.span,
                        kind: symbol.kind,
                    }),
                },
                None => self.errors.push(ResolveError::UndefinedSymbol {
                    name: *name,
                    use_span: expr.span,
                }),
            },
            ExprKind::Group(e) => self.resolve_expr(*e, in_scope),
            ExprKind::Unary { expr, .. } => self.resolve_expr(*expr, in_scope),
            ExprKind::Binary { lhs, rhs, .. } => {
                self.resolve_expr(*lhs, in_scope);
                self.resolve_expr(*rhs, in_scope);
            }
            ExprKind::Assign { tgt, val, .. } => {
                self.resolve_expr(*tgt, in_scope);
                self.resolve_expr(*val, in_scope);
            }

            ExprKind::Return(e) => self.resolve_expr(*e, in_scope),
            ExprKind::Break(e) => self.resolve_expr(*e, in_scope),
            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(*cond, in_scope);
                self.resolve_expr(*then_branch, in_scope);

                if let Some(e) = else_branch {
                    self.resolve_expr(*e, in_scope);
                }
            }
            ExprKind::Block(stmts) => {
                let block_scope =
                    self.symbol_table
                        .new_expr_scope(ScopeKind::Block, expr_id, in_scope);

                for stmt in stmts {
                    self.resolve_stmt(*stmt, block_scope);
                }
            }
            ExprKind::While { cond, body } => {
                self.resolve_expr(*cond, in_scope);

                let loop_scope =
                    self.symbol_table
                        .new_expr_scope(ScopeKind::Loop, expr_id, in_scope);

                self.resolve_expr(*body, loop_scope);
            }
            ExprKind::Match { expr, arms } => todo!(),
            ExprKind::Call { callee, args } => {
                self.resolve_expr(*callee, in_scope);

                for arg in args {
                    self.resolve_expr(*arg, in_scope);
                }
            }
            ExprKind::Field { object, .. } => {
                self.resolve_expr(*object, in_scope);
            }
            ExprKind::Index { object, index } => {
                self.resolve_expr(*object, in_scope);
                self.resolve_expr(*index, in_scope);
            }
            ExprKind::Loop(body) => {
                let loop_scope =
                    self.symbol_table
                        .new_expr_scope(ScopeKind::Loop, expr_id, in_scope);

                self.resolve_expr(*body, loop_scope);
            }
        }
    }

    fn resolve_type(&mut self, type_id: TypeId, in_scope: ScopeId) {
        let ty = &self.ast.types[type_id];

        match &ty.kind {
            TypeKind::Int
            | TypeKind::Float
            | TypeKind::Bool
            | TypeKind::String
            | TypeKind::Void
            | TypeKind::Never
            | TypeKind::Any => {}

            TypeKind::Path(path) => match self.symbol_table.lookup(*path, in_scope) {
                Some((symbol, _)) => match symbol.kind {
                    SymbolKind::Type => {}
                    _ => self.errors.push(ResolveError::TypeNotFound {
                        name: *path,
                        use_span: ty.span,
                    }),
                },
                None => self.errors.push(ResolveError::TypeNotFound {
                    name: *path,
                    use_span: ty.span,
                }),
            },

            TypeKind::Optional(inner) => {
                self.resolve_type(*inner, in_scope);
            }
            TypeKind::Array { ty, len } => {
                self.resolve_type(*ty, in_scope);
                self.resolve_expr(*len, in_scope); // Array length expression
            }
            TypeKind::DynArray(inner) => {
                self.resolve_type(*inner, in_scope);
            }
            TypeKind::Function { params, ret } => {
                for param_ty in params {
                    self.resolve_type(*param_ty, in_scope);
                }
                self.resolve_type(*ret, in_scope);
            }
        }
    }
}
