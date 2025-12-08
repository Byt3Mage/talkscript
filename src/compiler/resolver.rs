use ahash::{AHashMap, AHashSet};

use crate::{
    arena::Interner,
    compiler::{
        ast::{
            AstArena, Expr, ExprKind, Ident, Item, ItemId, ItemKind, Pattern, PatternKind, Stmt,
            StmtKind, Type, Type, TypeId,
        },
        tokens::Span,
    },
};

#[derive(Debug)]
pub enum ResolveErrorKind {
    DuplicateSymbol {
        name: Ident,
        first_defined: Span,
    },
    DuplicateField {
        struct_name: Ident,
        field_name: Ident,
    },
    UndefinedVariable(Ident),
    UndefinedType(Ident),
    CannotAssignToImmutable(Ident),
    InvalidAssignmentTarget,
    ResolverFailed,
}

pub struct ResolveError {
    pub kind: ResolveErrorKind,
    pub span: Span,
}

impl ResolveError {
    pub fn display(&self, interner: &Interner) -> String {
        match self.kind {
            ResolveErrorKind::DuplicateSymbol {
                name,
                first_defined,
            } => {
                let name_str = interner.resolve(name).unwrap_or("?");
                format!(
                    "Symbol '{}' is already defined at {:?}",
                    name_str, first_defined
                )
            }
            ResolveErrorKind::DuplicateField {
                struct_name,
                field_name,
                ..
            } => {
                let struct_str = interner.resolve(struct_name).unwrap_or("?");
                let field_str = interner.resolve(field_name).unwrap_or("?");
                format!("Duplicate field '{}' in struct '{}'", field_str, struct_str)
            }
            ResolveErrorKind::UndefinedVariable(name) => {
                let name_str = interner.resolve(name).unwrap_or("?");
                format!("Undefined variable '{}'", name_str)
            }
            ResolveErrorKind::UndefinedType(name) => {
                let name_str = interner.resolve(name).unwrap_or("?");
                format!("Undefined type '{}'", name_str)
            }
            ResolveErrorKind::CannotAssignToImmutable(name) => {
                let name_str = interner.resolve(name).unwrap_or("?");
                format!("Cannot assign to immutable variable '{}'", name_str)
            }
            ResolveErrorKind::InvalidAssignmentTarget => "Invalid assignment target".to_string(),
            ResolveErrorKind::ResolverFailed => {
                "Resolver did not return to global scope".to_string()
            }
        }
    }
}
#[derive(Debug)]
pub enum SymbolKind {
    Variable {
        ty: TypeId,
        mutable: bool,
    },
    Function {
        scope_id: ScopeId,
        params: Vec<TypeId>,
        ret: TypeId,
    },
    Type {
        ty: TypeId,
    },
    Module {
        scope_id: ScopeId,
    },
    Struct {
        fields: Vec<StructField>,
    },
}

#[derive(Debug)]
pub struct StructField {
    pub name: Ident,
    pub ty: TypeId,
}

#[derive(Debug)]
pub struct Symbol {
    pub name: Ident,
    pub kind: SymbolKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Scope {
    pub symbols: AHashMap<Ident, Symbol>,
    pub parent: Option<ScopeId>,
}

pub type ScopeId = usize;

#[derive(Debug)]
pub struct SymbolTable {
    pub scopes: Vec<Scope>,
    pub current: ScopeId,
}

impl SymbolTable {
    pub fn new() -> Self {
        let global = Scope {
            symbols: AHashMap::new(),
            parent: None,
        };

        Self {
            scopes: vec![global],
            current: 0,
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) -> ScopeId {
        self.scopes.push(Scope {
            symbols: AHashMap::new(),
            parent: Some(self.current),
        });
        self.current = self.scopes.len() - 1;
        self.current
    }

    // Exit current scope
    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current].parent {
            self.current = parent;
        } else {
            todo!("Cannot exit global scope")
        }
    }

    pub fn current_scope(&self) -> ScopeId {
        self.current
    }

    pub fn set_current_scope(&mut self, scope_id: ScopeId) {
        assert!(scope_id < self.scopes.len(), "Invalid scope ID");
        self.current = scope_id;
    }

    /// Define a symbol in current scope
    pub fn define(&mut self, symbol: Symbol) -> Result<(), ResolveError> {
        let name = symbol.name.clone();

        if let Some(existing) = self.scopes[self.current].symbols.get(&name) {
            return Err(ResolveError {
                kind: ResolveErrorKind::DuplicateSymbol {
                    name,
                    first_defined: existing.span,
                },
                span: symbol.span,
            });
        }

        self.scopes[self.current].symbols.insert(name, symbol);
        Ok(())
    }

    /// Lookup a symbol, searching parent scopes
    pub fn lookup(&self, name: &Ident) -> Option<&Symbol> {
        let mut scope_id = self.current;

        loop {
            let scope = &self.scopes[scope_id];

            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }

            scope_id = scope.parent?;
        }
    }

    /// Lookup in current scope only (E.g. checking duplicates)
    pub fn lookup_current(&self, name: &Ident) -> Option<&Symbol> {
        self.scopes[self.current].symbols.get(&name)
    }

    /// Lookup a symbol in a specific scope
    fn lookup_in_scope(&self, scope_id: ScopeId, name: &Ident) -> Option<&Symbol> {
        self.scopes.get(scope_id)?.symbols.get(&name)
    }

    /// Lookup a symbol starting from a specific scope (with parent traversal)
    fn lookup_from_scope(&self, mut scope_id: ScopeId, name: &Ident) -> Option<&Symbol> {
        loop {
            let scope = self.scopes.get(scope_id)?;

            if let Some(symbol) = scope.symbols.get(&name) {
                return Some(symbol);
            }

            scope_id = scope.parent?;
        }
    }

    pub fn set_var_type(&mut self, name: Ident, new_ty: TypeId) -> Result<(), ResolveError> {
        let mut scope_id = self.current;

        while scope_id < self.scopes.len() {
            let scope = &mut self.scopes[scope_id];

            if let Some(symbol) = scope.symbols.get_mut(&name) {
                if let SymbolKind::Variable { ty, .. } = &mut symbol.kind {
                    return Ok(*ty = new_ty);
                } else {
                    break;
                }
            }

            scope_id = match scope.parent {
                Some(id) => id,
                None => break,
            }
        }

        return Err(ResolveError {
            kind: ResolveErrorKind::UndefinedVariable(name),
            span: Span::default(),
        });
    }
}

pub struct Resolver<'a> {
    ast: &'a AstArena,
    pub symbols: SymbolTable,
    pub errors: Vec<ResolveError>,
}

impl<'a> Resolver<'a> {
    pub fn new(ast: &'a AstArena) -> Self {
        Self {
            ast,
            symbols: SymbolTable::new(),
            errors: vec![],
        }
    }

    pub fn resolve_package(&mut self, items: &[ItemId], span: Span) -> Result<(), ResolveError> {
        for item in items {
            self.resolve_item(&self.ast.items[*item]);
        }

        if self.symbols.current_scope() != 0 {
            self.errors.push(ResolveError {
                kind: ResolveErrorKind::ResolverFailed,
                span,
            });
        }

        Ok(())
    }
    pub fn resolve_item(&mut self, item: &Item) -> Result<(), ResolveError> {
        match &item.kind {
            ItemKind::Module { name, items } => {
                let parent_scope = self.symbols.current_scope();
                let module_scope = self.symbols.enter_scope();

                self.symbols.set_current_scope(parent_scope);

                if let Err(e) = self.symbols.define(Symbol {
                    name: *name,
                    kind: SymbolKind::Module {
                        scope_id: module_scope,
                    },
                    span: item.span,
                }) {
                    self.errors.push(e);
                }

                self.symbols.set_current_scope(module_scope);

                for item in items {
                    self.resolve_item(&self.ast.items[*item]);
                }

                self.symbols.set_current_scope(parent_scope);
            }
            ItemKind::Function {
                name,
                params,
                ret,
                body,
            } => {
                let parent_scope = self.symbols.current_scope();
                let function_scope = self.symbols.enter_scope();

                // Define function in current scope
                self.symbols.set_current_scope(parent_scope);

                let param_types = params.iter().map(|p| p.ty.clone()).collect();
                if let Err(e) = self.symbols.define(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Function {
                        scope_id: function_scope,
                        params: param_types,
                        ret: ret.clone(),
                    },
                    span: item.span,
                }) {
                    self.errors.push(e);
                }

                // Enter function scope
                self.symbols.set_current_scope(function_scope);

                for param in params {
                    if let Err(e) = self.symbols.define(Symbol {
                        name: param.name.clone(),
                        kind: SymbolKind::Variable {
                            ty: param.ty.clone(),
                            mutable: param.mutable,
                        },
                        span: param.span,
                    }) {
                        self.errors.push(e);
                    }
                }

                self.resolve_expr(&self.ast.exprs[*body]);

                //Exit function scope
                self.symbols.set_current_scope(parent_scope);
            }
            ItemKind::Struct { name, fields } => {
                let struct_fields = fields
                    .iter()
                    .map(|f| StructField {
                        name: f.name.clone(),
                        ty: f.ty.clone(),
                    })
                    .collect::<Vec<_>>();

                let mut seen = AHashSet::new();
                for field in &struct_fields {
                    if !seen.insert(field.name) {
                        self.errors.push(ResolveError {
                            kind: ResolveErrorKind::DuplicateField {
                                struct_name: *name,
                                field_name: field.name,
                            },
                            span: item.span,
                        })
                    }
                }

                if let Err(e) = self.symbols.define(Symbol {
                    name: *name,
                    kind: SymbolKind::Struct {
                        fields: struct_fields,
                    },
                    span: item.span,
                }) {
                    self.errors.push(e);
                }
            }
        }

        Ok(())
    }
    fn resolve_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if self.symbols.lookup(name).is_none() {
                    self.errors.push(ResolveError {
                        kind: ResolveErrorKind::UndefinedVariable(*name),
                        span: expr.span,
                    });
                }
            }
            ExprKind::Block(stmts) => {
                self.symbols.enter_scope();
                for stmt in stmts {
                    self.resolve_stmt(&self.ast.stmts[*stmt]);
                }
                self.symbols.exit_scope();
            }
            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(&self.ast.exprs[*cond]);
                self.resolve_expr(&self.ast.exprs[*then_branch]);
                if let Some(e) = else_branch {
                    self.resolve_expr(&self.ast.exprs[*e]);
                }
            }
            ExprKind::While { cond, body } => {
                self.resolve_expr(&self.ast.exprs[*cond]);
                self.resolve_expr(&self.ast.exprs[*body]);
            }
            ExprKind::Loop(body) => {
                self.resolve_expr(&self.ast.exprs[*body]);
            }
            ExprKind::Binary { lhs, rhs, .. } => {
                self.resolve_expr(&self.ast.exprs[*lhs]);
                self.resolve_expr(&self.ast.exprs[*rhs]);
            }
            ExprKind::Unary { expr, .. } => {
                self.resolve_expr(&self.ast.exprs[*expr]);
            }
            ExprKind::Assign { tgt, val, .. } => {
                let tgt = &self.ast.exprs[*tgt];
                self.resolve_expr(tgt);

                self.resolve_expr(&self.ast.exprs[*val]);

                // Check if target is mutable (if it's a variable)
                if let ExprKind::Ident(name) = &tgt.kind {
                    if let Some(symbol) = self.symbols.lookup(name)
                        && let SymbolKind::Variable { mutable, .. } = symbol.kind
                    {
                        if !mutable {
                            self.errors.push(ResolveError {
                                kind: ResolveErrorKind::CannotAssignToImmutable(*name),
                                span: expr.span,
                            });
                        }
                    } else {
                        self.errors.push(ResolveError {
                            kind: ResolveErrorKind::InvalidAssignmentTarget,
                            span: tgt.span,
                        });
                    }
                } else if !tgt.is_assignable() {
                    self.errors.push(ResolveError {
                        kind: ResolveErrorKind::InvalidAssignmentTarget,
                        span: tgt.span,
                    });
                }
            }
            ExprKind::Call { callee, args } => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            ExprKind::Field { object, .. } => {
                self.resolve_expr(object);
            }
            ExprKind::Index { object, index } => {
                self.resolve_expr(object);
                self.resolve_expr(index);
            }
            ExprKind::Group(expr) => {
                self.resolve_expr(expr);
            }
            ExprKind::Return(expr) => {
                self.resolve_expr(expr);
            }
            ExprKind::Break(expr) => {
                self.resolve_expr(expr);
            }
            ExprKind::ArrayLit(elements) => {
                for elem in elements {
                    self.resolve_expr(elem);
                }
            }
            ExprKind::Match { expr, arms } => {
                self.resolve_expr(expr);
                for arm in arms {
                    self.symbols.enter_scope();

                    self.resolve_pattern(&arm.pattern);

                    if let Some(guard) = &arm.guard {
                        self.resolve_expr(guard);
                    }

                    self.resolve_expr(&arm.body);

                    self.symbols.exit_scope();
                }
            }
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::String(_)
            | ExprKind::Null
            | ExprKind::Void
            | ExprKind::Continue => {}
        }
    }
    fn resolve_stmt(&mut self, stmt: &Stmt, ast: &AstArena) {
        match &stmt.kind {
            StmtKind::VarDecl {
                mutable,
                name,
                ty,
                val: value,
            } => {
                if let Some(v) = value {
                    self.resolve_expr(&self.ast.exprs[*v]);
                }

                // Determine type (inference happens in type checker)
                let val_ty = match ty {
                    Some(ty) => *ty,
                    None => ast.types.insert(Type {
                        kind: Type::Unknown,
                        span: stmt.span,
                    }),
                };

                // Define variable
                if let Err(e) = self.symbols.define(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Variable {
                        ty: val_ty,
                        mutable: false,
                    },
                    span: stmt.span,
                }) {
                    self.errors.push(e);
                }
            }

            StmtKind::Expr { expr, .. } => {
                self.resolve_expr(&ast.exprs[*expr]);
            }

            StmtKind::Semi => {}
        }
    }

    fn resolve_pattern(&mut self, pattern: &Pattern) {
        match &*pattern.kind {
            PatternKind::Wildcard => {}
            PatternKind::Variable(name) => {
                if let Err(e) = self.symbols.define(Symbol {
                    name: *name,
                    kind: SymbolKind::Variable {
                        ty: Type {
                            kind: Box::new(Type::Unknown),
                            span: Span::default(),
                        },
                        mutable: false,
                    },
                    span: pattern.span,
                }) {
                    self.errors.push(e);
                }
            }
            PatternKind::Destructure { base, fields } => {
                if self.symbols.lookup(base).is_none() {
                    self.errors.push(ResolveError {
                        kind: ResolveErrorKind::UndefinedType(*base),
                        span: pattern.span,
                    });
                }

                for (_, field_pattern) in fields {
                    self.resolve_pattern(field_pattern);
                }
            }
            PatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    self.resolve_pattern(pattern);
                }
            }
        }
    }

    pub fn lookup_qualified(&self, path: &[Ident]) -> Option<&Symbol> {
        if path.is_empty() {
            return None;
        }

        let mut symbol = self.symbols.lookup(&path[0])?;

        for component in &path[1..] {
            match &symbol.kind {
                SymbolKind::Module { scope_id } => {
                    symbol = self.symbols.lookup_in_scope(*scope_id, component)?;
                }
                _ => return None,
            }
        }

        Some(symbol)
    }
}
