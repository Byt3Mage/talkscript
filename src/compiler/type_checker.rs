use std::{collections::hash_map::Entry, rc::Rc};

use ahash::{AHashMap, AHashSet};

use crate::{
    arena::{Ident, Interner},
    compiler::{
        ast::{
            AstArena, AstTypeId, AstTypeKind, BinaryOp, ExprId, ExprKind, Field, GenericArg,
            GenericParams, Item, ItemId, ItemKind, Param, Path, PatternId, PatternKind, UnaryOp,
            Variant,
        },
        name_resolver::{ScopeId, SymbolKind, SymbolTable},
        tokens::Span,
        type_error::TypeError,
        type_info::{
            EnumInfo, FieldInfo, StructInfo, TypeArena, TypeId, TypeValue, UnionInfo, VariantInfo,
        },
    },
};

pub struct BuiltinTypes {
    pub int: TypeId,
    pub uint: TypeId,
    pub float: TypeId,
    pub bool: TypeId,
    pub char: TypeId,
    pub str: TypeId,
    pub cstr: TypeId,
    pub void: TypeId,
    pub never: TypeId,
}

impl BuiltinTypes {
    fn new(types: &mut TypeArena) -> Self {
        Self {
            int: types.insert(TypeValue::Int),
            uint: types.insert(TypeValue::Uint),
            float: types.insert(TypeValue::Float),
            bool: types.insert(TypeValue::Bool),
            char: types.insert(TypeValue::Char),
            str: types.insert(TypeValue::Str),
            cstr: types.insert(TypeValue::Cstr),
            void: types.insert(TypeValue::Void),
            never: types.insert(TypeValue::Never),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstValue {
    Int(i64),
    Uint(u64),
    Bool(bool),
    Char(char),
    // TODO: support more types
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeArg {
    Type(TypeId),
    Const(ConstValue),
}

pub struct TypeChecker<'a> {
    ast: &'a AstArena,
    symbols: &'a SymbolTable,
    interner: &'a Interner,

    // Type storage
    types: TypeArena,
    builtins: BuiltinTypes,

    // Non-generic type declarations -> TypeId
    type_decls: AHashMap<ItemId, TypeId>,

    // Monomorphization cache: (base_item, type_args) -> TypeId
    mono_cache: AHashMap<(ItemId, Rc<[TypeArg]>), TypeId>,

    // Currently resolving (for cycle detection)
    resolving: AHashSet<(ItemId, Rc<[TypeArg]>)>,

    // Scopes for tracking variable types
    local_scopes: Vec<AHashMap<Ident, TypeId>>,

    // Type substitutions for current generic context
    // Maps generic param index -> concrete TypeId
    substitutions: Rc<[TypeArg]>,

    // Maps generic param name to index in substitutions
    // Stack because generics can nest (e.g., method inside generic struct)
    generic_params: Vec<AHashMap<Ident, usize>>,

    // Results
    expr_types: AHashMap<ExprId, TypeId>,
    pattern_types: AHashMap<PatternId, TypeId>,

    // Interning caches for structural types
    pointer_cache: AHashMap<(TypeId, bool), TypeId>, // (pointee, mutable) -> TypeId
    optional_cache: AHashMap<TypeId, TypeId>,        // inner -> TypeId
    array_cache: AHashMap<(TypeId, usize), TypeId>,  // (elem, len) -> TypeId
    slice_cache: AHashMap<TypeId, TypeId>,           // elem -> TypeId
    tuple_cache: AHashMap<Rc<[TypeId]>, TypeId>,     // elems -> TypeId
    function_cache: AHashMap<(Rc<[TypeId]>, TypeId), TypeId>, // (params, ret) -> TypeId

    errors: Vec<TypeError>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(ast: &'a AstArena, symbols: &'a SymbolTable, interner: &'a Interner) -> Self {
        let mut types = TypeArena::new();
        let builtins = BuiltinTypes::new(&mut types);

        Self {
            ast,
            symbols,
            interner,
            types,
            builtins,
            type_decls: AHashMap::new(),
            mono_cache: AHashMap::new(),
            resolving: AHashSet::new(),
            local_scopes: Vec::new(),
            substitutions: Rc::new([]),
            generic_params: Vec::new(),
            expr_types: AHashMap::new(),
            pattern_types: AHashMap::new(),
            pointer_cache: AHashMap::new(),
            optional_cache: AHashMap::new(),
            array_cache: AHashMap::new(),
            slice_cache: AHashMap::new(),
            tuple_cache: AHashMap::new(),
            function_cache: AHashMap::new(),
            errors: Vec::new(),
        }
    }

    fn error(&mut self, err: TypeError) {
        self.errors.push(err);
    }

    fn push_scope(&mut self) {
        self.local_scopes.push(AHashMap::new());
    }

    fn pop_scope(&mut self) {
        self.local_scopes.pop();
    }

    fn push_generic_params(&mut self, generics: &GenericParams) {
        let map: AHashMap<Ident, usize> = generics
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.name, i))
            .collect();

        self.generic_params.push(map);
    }

    fn pop_generic_params(&mut self) {
        self.generic_params.pop();
    }

    fn define_local(&mut self, name: Ident, ty: TypeId) {
        if let Some(scope) = self.local_scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn lookup_local(&self, name: Ident) -> Option<TypeId> {
        for scope in self.local_scopes.iter().rev() {
            if let Some(&ty) = scope.get(&name) {
                return Some(ty);
            }
        }
        None
    }

    pub fn check_module(&mut self, items: &[ItemId], scope: ScopeId) {
        // First pass: register type declarations (creates placeholders)
        for &item_id in items {
            self.register_type_decl(item_id);
        }

        // Second pass: resolve type bodies and check expressions
        for &item_id in items {
            self.check_item(item_id, scope);
        }
    }

    fn register_type_decl(&mut self, item_id: ItemId) {
        let item = &self.ast.items[item_id];

        // Generics are instantiated on demand
        match &item.kind {
            // No type declarations
            ItemKind::Function { .. } | ItemKind::Const { .. } | ItemKind::Import(_) => {}

            ItemKind::Struct { generics, .. } => {
                if generics.params.is_empty() {
                    let type_id = self.types.insert(TypeValue::Incomplete);
                    self.type_decls.insert(item_id, type_id);
                }
            }

            ItemKind::Union { generics, .. } => {
                if generics.params.is_empty() {
                    let type_id = self.types.insert(TypeValue::Incomplete);
                    self.type_decls.insert(item_id, type_id);
                }
            }

            ItemKind::Enum { .. } => {
                let type_id = self.types.insert(TypeValue::Incomplete);
                self.type_decls.insert(item_id, type_id);
            }

            ItemKind::TypeAlias { generics, .. } => {
                if generics.params.is_empty() {
                    let type_id = self.types.insert(TypeValue::Incomplete);
                    self.type_decls.insert(item_id, type_id);
                }
            }

            ItemKind::Module { items } => {
                for &nested_item_id in items {
                    self.register_type_decl(nested_item_id);
                }
            }
        }
    }

    // ========================================================================
    // Second Pass: Check Items
    // ========================================================================

    fn check_item(&mut self, item_id: ItemId, scope: ScopeId) {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Module { items } => {
                self.check_module(items, ScopeId::Item(item_id));
            }

            ItemKind::Struct { generics, fields } => {
                if generics.is_empty() {
                    let type_id = self.type_decls[&item_id];
                    let info = self.create_struct(item.name, fields, scope);
                    *self.types.get_mut(type_id) = TypeValue::Struct(info);
                }
            }

            ItemKind::Union { generics, fields } => {
                if generics.is_empty() {
                    let type_id = self.type_decls[&item_id];
                    let info = self.create_union(item.name, fields, scope);
                    *self.types.get_mut(type_id) = TypeValue::Union(info);
                }
            }

            ItemKind::Enum { variants } => {
                let type_id = self.type_decls[&item_id];
                let info = self.create_enum(item.name, variants);
                *self.types.get_mut(type_id) = TypeValue::Enum(info);
            }

            ItemKind::TypeAlias { generics, ty } => {
                if generics.params.is_empty() {
                    let type_id = self.type_decls[&item_id];
                    let alias = self.create_alias(*ty, scope);
                    *self.types.get_mut(type_id) = alias
                }
            }

            ItemKind::Function {
                generics,
                params,
                ret,
                body,
            } => {
                if generics.params.is_empty() {
                    self.check_function(item_id, params, *ret, *body);
                }
            }

            ItemKind::Const { ty, value } => {
                self.check_const(*ty, *value, scope);
            }

            ItemKind::Import(_) => {}
        }
    }

    fn create_struct(&mut self, name: Ident, fields: &[Field], scope: ScopeId) -> StructInfo {
        let fields = fields
            .iter()
            .map(|f| FieldInfo {
                name: f.name,
                ty: self.resolve_ast_type(f.ty, scope),
            })
            .collect();

        StructInfo { name, fields }
    }

    fn create_union(&mut self, name: Ident, fields: &[Field], scope: ScopeId) -> UnionInfo {
        let fields = fields
            .iter()
            .map(|f| FieldInfo {
                name: f.name,
                ty: self.resolve_ast_type(f.ty, scope),
            })
            .collect();

        UnionInfo { name, fields }
    }

    fn create_enum(&mut self, name: Ident, variants: &[Variant]) -> EnumInfo {
        let mut seen_values: AHashMap<i64, Span> = AHashMap::new();
        let mut next_value: i64 = 0;

        let mut variant_infos = Vec::new();
        for variant in variants {
            let value = if let Some(expr_id) = variant.value {
                // Evaluate constant expression
                match self.eval_const_expr(expr_id) {
                    Some(v) => match v {
                        ConstValue::Int(n) => {
                            next_value = n + 1;
                            n
                        }
                        _ => {
                            self.error(TypeError::TypeMismatch {
                                expected: self.builtins.int,
                                found: self.const_value_type(&v),
                                span: variant.span,
                            });
                            next_value
                        }
                    },
                    None => {
                        // Error already reported by eval_const_expr
                        next_value += 1;
                        next_value - 1
                    }
                }
            } else {
                let v = next_value;
                next_value += 1;
                v
            };

            // Check for duplicate values
            match seen_values.entry(value) {
                Entry::Occupied(entry) => self.error(TypeError::DuplicateEnumValue {
                    name: variant.name,
                    value,
                    first_def: *entry.get(),
                    dupe_def: variant.span,
                }),
                Entry::Vacant(entry) => {
                    entry.insert(variant.span);
                }
            }

            variant_infos.push(VariantInfo {
                name: variant.name,
                value,
            });
        }

        EnumInfo {
            name,
            variants: variant_infos.into(),
        }
    }

    fn create_alias(&mut self, ty: AstTypeId, scope: ScopeId) -> TypeValue {
        let resolved = self.resolve_ast_type(ty, scope);
        self.types.get(resolved).clone()
    }

    fn check_const(&mut self, ty: Option<AstTypeId>, value: ExprId, scope: ScopeId) {
        let value_ty = self.check_expr(value, scope);

        if let Some(ast_ty) = ty {
            let const_ty = self.resolve_ast_type(ast_ty, scope);
            self.expect_assignable(value_ty, const_ty, self.ast.exprs[value].span);
        }
    }

    fn check_function(
        &mut self,
        item_id: ItemId,
        params: &[Param],
        ret: Option<AstTypeId>,
        body: ExprId,
    ) {
        self.push_scope();

        let func_scope = ScopeId::Item(item_id);

        for param in params {
            let param_ty = self.resolve_ast_type(param.ty, func_scope);
            self.bind_pattern(param.pattern, param_ty);
        }

        let ret_ty = ret.map_or(self.builtins.void, |r| self.resolve_ast_type(r, func_scope));
        let body_ty = self.check_expr(body, func_scope);
        self.expect_assignable(body_ty, ret_ty, self.ast.exprs[body].span);

        self.pop_scope();
    }

    fn bind_pattern(&mut self, pattern_id: PatternId, ty: TypeId) {
        let pattern = &self.ast.patterns[pattern_id];
        self.pattern_types.insert(pattern_id, ty);

        match &pattern.kind {
            PatternKind::Wildcard => {}

            PatternKind::Binding { name, .. } => {
                self.define_local(*name, ty);
            }

            PatternKind::Tuple(elements) => {
                let elem_tys = match self.types.get(ty) {
                    TypeValue::Tuple(tys) => tys.clone(),
                    _ => {
                        self.error(TypeError::TypeMismatch {
                            expected: ty, // TODO: better error
                            found: ty,
                            span: pattern.span,
                        });
                        return;
                    }
                };

                for (elem_pat, elem_ty) in elements.iter().zip(elem_tys.iter()) {
                    self.bind_pattern(*elem_pat, *elem_ty);
                }
            }

            PatternKind::Struct { path, fields, .. } => {
                let field_tys = match self.types.get(ty) {
                    TypeValue::Struct(info) => info.fields.clone(),
                    _ => {
                        self.error(TypeError::NotAStruct {
                            ty,
                            span: pattern.span,
                        });
                        return;
                    }
                };

                for field_pat in fields {
                    let field_ty = field_tys
                        .iter()
                        .find(|f| f.name == field_pat.name)
                        .map(|f| f.ty);

                    match field_ty {
                        Some(ty) => self.bind_pattern(field_pat.pattern, ty),
                        None => {
                            self.error(TypeError::FieldNotFound {
                                ty,
                                field: field_pat.name,
                                span: field_pat.span,
                            });
                        }
                    }
                }
            }

            PatternKind::Union { path, field } => {
                let field_ty = match self.types.get(ty) {
                    TypeValue::Union(info) => info
                        .fields
                        .iter()
                        .find(|f| f.name == field.name)
                        .map(|f| f.ty),
                    _ => {
                        self.error(TypeError::NotAUnion {
                            ty,
                            span: pattern.span,
                        });
                        return;
                    }
                };

                match field_ty {
                    Some(ty) => self.bind_pattern(field.pattern, ty),
                    None => {
                        self.error(TypeError::FieldNotFound {
                            ty,
                            field: field.name,
                            span: field.span,
                        });
                    }
                }
            }

            // Literals and paths don't bind anything
            PatternKind::Int(_)
            | PatternKind::Float(_)
            | PatternKind::Bool(_)
            | PatternKind::Char(_)
            | PatternKind::CStr(_)
            | PatternKind::Path(_) => {}

            PatternKind::Array(_) => todo!("array pattern"),

            PatternKind::AnonStruct { fields, .. } => {
                // Same as Struct but type must match structurally
                // (more complex - defer for now)
            }

            PatternKind::AnonUnion { field } => {
                // Same as Union but type must match structurally
            }

            PatternKind::Or(patterns) => {
                // All branches must bind same names with same types
                // Just check first one for now
                if let Some(&first) = patterns.first() {
                    self.bind_pattern(first, ty);
                }
            }

            PatternKind::Range { .. } => {
                // No bindings
            }
        }
    }
    fn resolve_ast_type(&mut self, ast_type_id: AstTypeId, scope: ScopeId) -> TypeId {
        let ast_type = &self.ast.types[ast_type_id];

        match &ast_type.kind {
            AstTypeKind::Path(path) => self.resolve_type_path(path, scope, ast_type.span),

            AstTypeKind::Pointer { pointee, mutable } => {
                let pointee_ty = self.resolve_ast_type(*pointee, scope);
                self.intern_pointer(pointee_ty, *mutable)
            }

            AstTypeKind::Optional(inner) => {
                let inner_ty = self.resolve_ast_type(*inner, scope);
                self.intern_optional(inner_ty)
            }

            AstTypeKind::Array { elem, size } => {
                let elem_ty = self.resolve_ast_type(*elem, scope);
                let len = match self.eval_const_expr(*size) {
                    Some(ConstValue::Uint(n)) => n as usize,
                    _ => {
                        self.error(TypeError::InvalidArrayLength {
                            span: ast_type.span,
                        });
                        0
                    }
                };
                self.intern_array(elem_ty, len)
            }

            AstTypeKind::Slice(elem) => {
                let elem_ty = self.resolve_ast_type(*elem, scope);
                self.intern_slice(elem_ty)
            }

            AstTypeKind::Tuple(elems) => {
                let elem_tys = elems
                    .iter()
                    .map(|&e| self.resolve_ast_type(e, scope))
                    .collect();
                self.intern_tuple(elem_tys)
            }

            AstTypeKind::Function { params, ret } => {
                let param_tys = params
                    .iter()
                    .map(|&p| self.resolve_ast_type(p, scope))
                    .collect();
                let ret_ty = self.resolve_ast_type(*ret, scope);
                self.intern_function(param_tys, ret_ty)
            }

            AstTypeKind::SelfType => {
                // Should have been resolved during impl checking
                // If we hit this, we're outside an impl block
                self.error(TypeError::SelfOutsideImpl {
                    span: ast_type.span,
                });
                self.builtins.never
            }

            AstTypeKind::Infer => {
                // Type inference placeholder - for now, error
                self.error(TypeError::CannotInfer {
                    span: ast_type.span,
                });
                self.builtins.never
            }

            AstTypeKind::AnonStruct(fields) => {
                let info = self.create_struct(self.interner.anon_name(), fields, scope);
                self.types.insert(TypeValue::Struct(info))
            }

            AstTypeKind::AnonUnion(fields) => {
                let info = self.create_union(self.interner.anon_name(), fields, scope);
                self.types.insert(TypeValue::Union(info))
            }

            AstTypeKind::AnonEnum(variants) => {
                let info = self.create_enum(self.interner.anon_name(), variants);
                self.types.insert(TypeValue::Enum(info))
            }
        }
    }

    fn resolve_type_path(&mut self, path: &Path, scope: ScopeId, span: Span) -> TypeId {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];

            // Check builtins first
            if segment.args.is_empty()
                && let Some(ty) = self.resolve_builtin(segment.name)
            {
                return ty;
            }

            // Check if its a generic parameter in scope
            if let Some(type_arg) = self.lookup_generic_param(segment.name) {
                match type_arg {
                    TypeArg::Type(ty) => {
                        let ty = *ty;
                        if !segment.args.is_empty() {
                            self.error(TypeError::GenericParamWithArgs { span });
                        }
                        return ty;
                    }
                    TypeArg::Const(_) => {
                        self.error(TypeError::NotAType { span });
                        return self.builtins.never;
                    }
                }
            }

            // Check in symbol table
            let symbol = match self.symbols.lookup(segment.name, scope) {
                Some((sym, _)) => sym,
                None => {
                    self.error(TypeError::UndefinedType {
                        name: segment.name,
                        span,
                    });
                    return self.builtins.never;
                }
            };

            let item_id = match symbol.kind {
                SymbolKind::Item(id) => id,
                _ => {
                    self.error(TypeError::NotAType { span });
                    return self.builtins.never;
                }
            };

            // Resolve type args
            let type_args = self.resolve_generic_args(&segment.args, scope);

            return self.resolve_type_item(item_id, type_args, span);
        }

        self.resolve_qualified_type_path(path, scope)
    }

    fn resolve_builtin(&self, name: Ident) -> Option<TypeId> {
        match self.interner.resolve(name)? {
            "int" => Some(self.builtins.int),
            "uint" => Some(self.builtins.uint),
            "float" => Some(self.builtins.float),
            "bool" => Some(self.builtins.bool),
            "char" => Some(self.builtins.char),
            "str" => Some(self.builtins.str),
            "cstr" => Some(self.builtins.cstr),
            "void" => Some(self.builtins.void),
            "!" => Some(self.builtins.never),
            _ => None,
        }
    }

    fn resolve_generic_args(&mut self, args: &[GenericArg], scope: ScopeId) -> Rc<[TypeArg]> {
        args.iter()
            .map(|arg| match arg {
                GenericArg::Type(ty) => TypeArg::Type(self.resolve_ast_type(*ty, scope)),
                GenericArg::Const(expr) => TypeArg::Const(self.eval_const_expr(*expr).unwrap()),
            })
            .collect()
    }

    fn resolve_qualified_type_path(&mut self, path: &Path, scope: ScopeId) -> TypeId {
        let mut current_scope = scope;

        for (i, segment) in path.segments.iter().enumerate() {
            let is_last = i == path.segments.len() - 1;

            // Look up this segment in current scope
            let symbol = match self.symbols.lookup(segment.name, current_scope) {
                Some((sym, _)) => sym,
                None => {
                    self.error(TypeError::UndefinedType {
                        name: segment.name,
                        span: segment.span,
                    });
                    return self.builtins.never;
                }
            };

            let item_id = match symbol.kind {
                SymbolKind::Item(id) => id,
                _ => {
                    self.error(TypeError::NotAType { span: segment.span });
                    return self.builtins.never;
                }
            };

            if is_last {
                // final segment, resolve as type with type args
                let type_args = self.resolve_generic_args(&segment.args, scope);
                return self.resolve_type_item(item_id, type_args, segment.span);
            }

            // Not last - must be a module
            match &self.ast.items[item_id].kind {
                ItemKind::Module { .. } => {
                    current_scope = ScopeId::Item(item_id);
                }
                _ => {
                    self.error(TypeError::NotAModule { span: segment.span });
                    return self.builtins.never;
                }
            }

            // Non-final segments shouldn't have type args
            if !segment.args.is_empty() {
                self.error(TypeError::TypeArgsOnModule { span: segment.span });
            }
        }

        self.builtins.never
    }

    fn resolve_type_item(
        &mut self,
        item_id: ItemId,
        type_args: Rc<[TypeArg]>,
        span: Span,
    ) -> TypeId {
        let item = &self.ast.items[item_id];

        let generics = match &item.kind {
            ItemKind::Struct { generics, .. } => generics,
            ItemKind::Union { generics, .. } => generics,
            ItemKind::TypeAlias { generics, .. } => generics,
            ItemKind::Enum { .. } => {
                if !type_args.is_empty() {
                    self.error(TypeError::WrongNumberOfTypeArgs {
                        expected: 0,
                        found: type_args.len(),
                        span,
                    });
                }

                return self
                    .type_decls
                    .get(&item_id)
                    .copied()
                    .unwrap_or(self.builtins.never);
            }
            _ => {
                self.error(TypeError::NotAType { span });
                return self.builtins.never;
            }
        };

        let expected_count = generics.params.len();
        let found_count = type_args.len();

        // Non-generic type
        if expected_count == 0 {
            if found_count != 0 {
                self.error(TypeError::WrongNumberOfTypeArgs {
                    expected: 0,
                    found: found_count,
                    span,
                });
            }
            return self
                .type_decls
                .get(&item_id)
                .copied()
                .unwrap_or(self.builtins.never);
        }

        // Generic type - need monomorphization
        if found_count != expected_count {
            self.error(TypeError::WrongNumberOfTypeArgs {
                expected: expected_count,
                found: found_count,
                span,
            });
            return self.builtins.never;
        }

        let cache_key = (item_id, Rc::clone(&type_args));

        if let Some(&type_id) = self.mono_cache.get(&cache_key) {
            return type_id;
        }

        // check for cycles
        if self.resolving.contains(&cache_key) {
            // Recursive type - return placeholder from type_decls
            return self
                .type_decls
                .get(&item_id)
                .copied()
                .unwrap_or(self.builtins.never);
        }

        // Mark as resolving
        self.resolving.insert(cache_key.clone());

        // Create placeholder and cache it
        let placeholder = self.types.insert(TypeValue::Incomplete);
        self.mono_cache.insert(cache_key.clone(), placeholder);

        // Save current substitutions, set new ones
        let old_subs = std::mem::replace(&mut self.substitutions, type_args.clone());

        self.push_generic_params(generics);

        // Resolve the type body with substitutions active
        let resolved_type = self.resolve_type_body(item_id, item);
        *self.types.get_mut(placeholder) = resolved_type;

        self.pop_generic_params();
        self.substitutions = old_subs;
        self.resolving.remove(&cache_key);

        placeholder
    }

    fn resolve_type_body(&mut self, item_id: ItemId, item: &Item) -> TypeValue {
        match &item.kind {
            ItemKind::Struct { fields, .. } => {
                TypeValue::Struct(self.create_struct(item.name, fields, ScopeId::Item(item_id)))
            }

            ItemKind::Union { fields, .. } => {
                TypeValue::Union(self.create_union(item.name, fields, ScopeId::Item(item_id)))
            }

            ItemKind::TypeAlias { ty, .. } => self.create_alias(*ty, ScopeId::Item(item_id)),

            // Enums aren't generic, but handle for completeness
            ItemKind::Enum { variants } => TypeValue::Enum(self.create_enum(item.name, variants)),

            // Shouldn't reach here - resolve_type_item already checked
            _ => TypeValue::Incomplete,
        }
    }

    fn lookup_generic_param(&self, name: Ident) -> Option<&TypeArg> {
        // Check if this name is a generic parameter in the current context
        // Generic params are tracked by index, substitutions holds the concrete types
        for scope in self.generic_params.iter().rev() {
            if let Some(&index) = scope.get(&name) {
                return self.substitutions.get(index);
            }
        }

        None
    }

    fn eval_const_expr(&mut self, expr_id: ExprId) -> Option<ConstValue> {
        let expr = &self.ast.exprs[expr_id];

        match &expr.kind {
            ExprKind::Int(v) => Some(ConstValue::Int(*v)),
            ExprKind::Uint(v) => Some(ConstValue::Uint(*v)),
            ExprKind::Bool(v) => Some(ConstValue::Bool(*v)),
            ExprKind::Char(v) => Some(ConstValue::Char(*v)),

            ExprKind::Path(path) => self.eval_const_path(path, expr.span),

            // Unary operations
            ExprKind::Unary { op, expr: inner } => {
                let val = self.eval_const_expr(*inner)?;
                self.eval_const_unary(*op, val, expr.span)
            }

            // Binary operations
            ExprKind::Binary { op, lhs, rhs } => {
                let left = self.eval_const_expr(*lhs)?;
                let right = self.eval_const_expr(*rhs)?;
                self.eval_const_binary(*op, left, right, expr.span)
            }

            // Grouping
            ExprKind::Group(inner) => self.eval_const_expr(*inner),

            // Everything else is not const-evaluable (for now)
            _ => {
                self.error(TypeError::NotConstExpr { span: expr.span });
                None
            }
        }
    }

    fn eval_const_path(&mut self, path: &Path, span: Span) -> Option<ConstValue> {
        // Only handle simple single-segment paths for now
        if path.segments.len() != 1 || !path.segments[0].args.is_empty() {
            self.error(TypeError::NotConstExpr { span });
            return None;
        }

        let name = path.segments[0].name;

        // Check if it's a const generic param
        if let Some(TypeArg::Const(val)) = self.lookup_generic_param(name) {
            return Some(val.clone());
        }

        // TODO: Look up const items in symbol table
        // For now, error
        self.error(TypeError::NotConstExpr { span });
        None
    }
    fn eval_const_unary(&mut self, op: UnaryOp, val: ConstValue, span: Span) -> Option<ConstValue> {
        match (op, val) {
            (UnaryOp::Neg, ConstValue::Int(v)) => Some(ConstValue::Int(-v)),
            (UnaryOp::Not, ConstValue::Bool(v)) => Some(ConstValue::Bool(!v)),
            (UnaryOp::BitNot, ConstValue::Int(v)) => Some(ConstValue::Int(!v)),
            (UnaryOp::BitNot, ConstValue::Uint(v)) => Some(ConstValue::Uint(!v)),
            _ => {
                self.error(TypeError::InvalidConstOp { span });
                None
            }
        }
    }

    fn eval_const_binary(
        &mut self,
        op: BinaryOp,
        lhs: ConstValue,
        rhs: ConstValue,
        span: Span,
    ) -> Option<ConstValue> {
        match (op, lhs, rhs) {
            (BinaryOp::Add, ConstValue::Int(a), ConstValue::Int(b)) => Some(ConstValue::Int(a + b)),
            (BinaryOp::Sub, ConstValue::Int(a), ConstValue::Int(b)) => Some(ConstValue::Int(a - b)),
            (BinaryOp::Mul, ConstValue::Int(a), ConstValue::Int(b)) => Some(ConstValue::Int(a * b)),
            (BinaryOp::Div, ConstValue::Int(a), ConstValue::Int(b)) => {
                if b == 0 {
                    self.error(TypeError::DivisionByZero { span });
                    None
                } else {
                    Some(ConstValue::Int(a / b))
                }
            }
            (BinaryOp::Mod, ConstValue::Int(a), ConstValue::Int(b)) => {
                if b == 0 {
                    self.error(TypeError::DivisionByZero { span });
                    None
                } else {
                    Some(ConstValue::Int(a % b))
                }
            }

            (BinaryOp::Add, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a + b))
            }
            (BinaryOp::Sub, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a - b))
            }
            (BinaryOp::Mul, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a * b))
            }
            (BinaryOp::Div, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                if b == 0 {
                    self.error(TypeError::DivisionByZero { span });
                    None
                } else {
                    Some(ConstValue::Uint(a / b))
                }
            }
            (BinaryOp::Mod, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                if b == 0 {
                    self.error(TypeError::DivisionByZero { span });
                    None
                } else {
                    Some(ConstValue::Uint(a % b))
                }
            }

            (BinaryOp::Eq, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Bool(a == b))
            }
            (BinaryOp::Ne, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Bool(a != b))
            }
            (BinaryOp::Lt, ConstValue::Int(a), ConstValue::Int(b)) => Some(ConstValue::Bool(a < b)),
            (BinaryOp::Le, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Bool(a <= b))
            }
            (BinaryOp::Gt, ConstValue::Int(a), ConstValue::Int(b)) => Some(ConstValue::Bool(a > b)),
            (BinaryOp::Ge, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Bool(a >= b))
            }

            (BinaryOp::Eq, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Bool(a == b))
            }
            (BinaryOp::Ne, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Bool(a != b))
            }
            (BinaryOp::Lt, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Bool(a < b))
            }
            (BinaryOp::Le, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Bool(a <= b))
            }
            (BinaryOp::Gt, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Bool(a > b))
            }
            (BinaryOp::Ge, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Bool(a >= b))
            }

            (BinaryOp::Eq, ConstValue::Bool(a), ConstValue::Bool(b)) => {
                Some(ConstValue::Bool(a == b))
            }
            (BinaryOp::Ne, ConstValue::Bool(a), ConstValue::Bool(b)) => {
                Some(ConstValue::Bool(a != b))
            }

            (BinaryOp::And, ConstValue::Bool(a), ConstValue::Bool(b)) => {
                Some(ConstValue::Bool(a && b))
            }
            (BinaryOp::Or, ConstValue::Bool(a), ConstValue::Bool(b)) => {
                Some(ConstValue::Bool(a || b))
            }

            (BinaryOp::BitAnd, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Int(a & b))
            }
            (BinaryOp::BitOr, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Int(a | b))
            }
            (BinaryOp::BitXor, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Int(a ^ b))
            }
            (BinaryOp::Shl, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Int(a << b))
            }
            (BinaryOp::Shr, ConstValue::Int(a), ConstValue::Int(b)) => {
                Some(ConstValue::Int(a >> b))
            }

            (BinaryOp::BitAnd, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a & b))
            }
            (BinaryOp::BitOr, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a | b))
            }
            (BinaryOp::BitXor, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a ^ b))
            }
            (BinaryOp::Shl, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a << b))
            }
            (BinaryOp::Shr, ConstValue::Uint(a), ConstValue::Uint(b)) => {
                Some(ConstValue::Uint(a >> b))
            }

            _ => {
                self.error(TypeError::InvalidConstOp { span });
                None
            }
        }
    }

    fn const_value_type(&self, value: &ConstValue) -> TypeId {
        match value {
            ConstValue::Int(_) => self.builtins.int,
            ConstValue::Uint(_) => self.builtins.uint,
            ConstValue::Bool(_) => self.builtins.bool,
            ConstValue::Char(_) => self.builtins.char,
        }
    }

    fn is_assignable(&self, source: TypeId, target: TypeId) -> bool {
        if source == target {
            return true;
        }

        let source_ty = self.types.get(source);
        let target_ty = self.types.get(target);

        // Check structural compatibility
        match (source_ty, target_ty) {
            // Never is assignable to anything (diverging expressions)
            (TypeValue::Never, _) => true,

            // Optional: T -> T? (value to optional)
            (_, TypeValue::Optional(inner)) => self.is_assignable(source, *inner),

            // Pointer coercion: @mut T -> @T (mutable to immutable)
            (
                TypeValue::Pointer {
                    pointee: src_pointee,
                    mutable: true,
                },
                TypeValue::Pointer {
                    pointee: tgt_pointee,
                    mutable: false,
                },
            ) => self.is_assignable(*src_pointee, *tgt_pointee),

            // Arrays must match exactly (element type and length)
            (
                TypeValue::Array {
                    elem: src_elem,
                    len: src_len,
                },
                TypeValue::Array {
                    elem: tgt_elem,
                    len: tgt_len,
                },
            ) => src_len == tgt_len && self.is_assignable(*src_elem, *tgt_elem),

            // Slices: element types must be assignable
            (TypeValue::Slice(src_elem), TypeValue::Slice(tgt_elem)) => {
                self.is_assignable(*src_elem, *tgt_elem)
            }

            // Array to slice coercion: [T; N] -> [T]
            (TypeValue::Array { elem: src_elem, .. }, TypeValue::Slice(tgt_elem)) => {
                self.is_assignable(*src_elem, *tgt_elem)
            }

            // Tuples: same arity, each element assignable
            (TypeValue::Tuple(src_elems), TypeValue::Tuple(tgt_elems)) => {
                src_elems.len() == tgt_elems.len()
                    && src_elems
                        .iter()
                        .zip(tgt_elems.iter())
                        .all(|(s, t)| self.is_assignable(*s, *t))
            }

            // Functions: contravariant params, covariant return
            (
                TypeValue::Function {
                    params: src_params,
                    ret: src_ret,
                },
                TypeValue::Function {
                    params: tgt_params,
                    ret: tgt_ret,
                },
            ) => {
                src_params.len() == tgt_params.len()
                           && src_params.iter().zip(tgt_params.iter())
                               .all(|(s, t)| self.is_assignable(*t, *s))  // contravariant
                           && self.is_assignable(*src_ret, *tgt_ret) // covariant
            }

            // Structs, unions, enums: must be same TypeId (nominal typing)
            // Already handled by source == target check above

            // No other coercions
            _ => false,
        }
    }

    fn join_types(&mut self, a: TypeId, b: TypeId, span: Span) -> TypeId {
        // Same type - trivial
        if a == b {
            return a;
        }

        let a_ty = self.types.get(a).clone();
        let b_ty = self.types.get(b).clone();

        // Never joins with anything to produce the other type
        if matches!(a_ty, TypeValue::Never) {
            return b;
        }

        if matches!(b_ty, TypeValue::Never) {
            return a;
        }

        // Optional joining: T and T? -> T?
        if let TypeValue::Optional(inner) = a_ty {
            if self.is_assignable(b, inner) {
                return a;
            }
        }

        if let TypeValue::Optional(inner) = b_ty {
            if self.is_assignable(a, inner) {
                return b;
            }
        }

        // Tuples: join element-wise
        if let (TypeValue::Tuple(a_elems), TypeValue::Tuple(b_elems)) = (a_ty, b_ty) {
            if a_elems.len() == b_elems.len() {
                let joined = a_elems
                    .iter()
                    .zip(b_elems.iter())
                    .map(|(&ae, &be)| self.join_types(ae, be, span))
                    .collect();

                // Check if any join failed (produced never unexpectedly)
                // For now, just create the tuple
                return self.types.insert(TypeValue::Tuple(joined));
            }
        }

        // No valid join - error
        self.error(TypeError::TypeJoinInvalid {
            first: a,
            second: b,
            span,
        });

        self.builtins.never
    }

    fn expect_assignable(&mut self, source: TypeId, target: TypeId, span: Span) {
        if !self.is_assignable(source, target) {
            self.error(TypeError::TypeMismatch {
                expected: target,
                found: source,
                span,
            });
        }
    }

    fn intern_pointer(&mut self, pointee: TypeId, mutable: bool) -> TypeId {
        let key = (pointee, mutable);
        if let Some(&id) = self.pointer_cache.get(&key) {
            return id;
        }

        let id = self.types.insert(TypeValue::Pointer { pointee, mutable });
        self.pointer_cache.insert(key, id);
        id
    }

    fn intern_optional(&mut self, inner: TypeId) -> TypeId {
        if let Some(&id) = self.optional_cache.get(&inner) {
            return id;
        }

        let id = self.types.insert(TypeValue::Optional(inner));
        self.optional_cache.insert(inner, id);
        id
    }

    fn intern_array(&mut self, elem: TypeId, len: usize) -> TypeId {
        let key = (elem, len);
        if let Some(&id) = self.array_cache.get(&key) {
            return id;
        }

        let id = self.types.insert(TypeValue::Array { elem, len });
        self.array_cache.insert(key, id);
        id
    }

    fn intern_slice(&mut self, elem: TypeId) -> TypeId {
        if let Some(&id) = self.slice_cache.get(&elem) {
            return id;
        }

        let id = self.types.insert(TypeValue::Slice(elem));
        self.slice_cache.insert(elem, id);
        id
    }

    fn intern_tuple(&mut self, elems: Rc<[TypeId]>) -> TypeId {
        if let Some(&id) = self.tuple_cache.get(&elems) {
            return id;
        }

        let id = self.types.insert(TypeValue::Tuple(elems.clone()));
        self.tuple_cache.insert(elems, id);
        id
    }

    fn intern_function(&mut self, params: Rc<[TypeId]>, ret: TypeId) -> TypeId {
        let key = (params.clone(), ret);
        if let Some(&id) = self.function_cache.get(&key) {
            return id;
        }

        let id = self.types.insert(TypeValue::Function { params, ret });
        self.function_cache.insert(key, id);
        id
    }

    fn check_expr(&mut self, expr_id: ExprId, scope: ScopeId) -> TypeId {
        let expr = &self.ast.exprs[expr_id];

        let ty = match &expr.kind {
            // Literals - trivial
            ExprKind::Int(_) => self.builtins.int,
            ExprKind::Uint(_) => self.builtins.uint,
            ExprKind::Float(_) => self.builtins.float,
            ExprKind::Bool(_) => self.builtins.bool,
            ExprKind::Char(_) => self.builtins.char,
            ExprKind::Cstr(_) => self.builtins.cstr,
            ExprKind::Void => self.builtins.void,

            // Path - variable or constant lookup
            ExprKind::Path(path) => self.check_path_expr(path, scope, expr.span),

            // Grouping - just check inner
            ExprKind::Group(inner) => self.check_expr(*inner, scope),

            // More to come...
            _ => {
                self.error(TypeError::NotImplemented { span: expr.span });
                self.builtins.never
            }
        };

        // Store the result
        self.expr_types.insert(expr_id, ty);
        ty
    }

    fn check_path_expr(&mut self, path: &Path, scope: ScopeId, span: Span) -> TypeId {
        // Simple single-segment path - variable lookup
        if path.is_single() {
            let name = path.segments[0].name;

            // Check locals first
            if let Some(ty) = self.lookup_local(name) {
                return ty;
            }

            // Check const generic params
            if let Some(TypeArg::Const(v)) = self.lookup_generic_param(name) {
                return self.const_value_type(v);
            }

            // Look up in symbol table
            if let Some((symbol, sym_scope)) = self.symbols.lookup(name, scope) {
                match symbol.kind {
                    SymbolKind::Item(item_id) => {
                        return self.check_item_as_expr(item_id, Rc::new([]), sym_scope, span);
                    }
                    SymbolKind::Variable => {
                        // Should have been in locals - error
                        self.error(TypeError::UndefinedVariable { name, span });
                        return self.builtins.never;
                    }
                    SymbolKind::GenericType | SymbolKind::GenericConst => {
                        self.error(TypeError::TypeUsedAsValue { span });
                        return self.builtins.never;
                    }
                }
            }

            self.error(TypeError::UndefinedVariable { name, span });
            return self.builtins.never;
        }

        // Multi-segment or with type args - qualified path
        self.check_qualified_path_expr(path, scope, span)
    }

    fn check_item_as_expr(
        &mut self,
        item_id: ItemId,
        type_args: Rc<[TypeArg]>,
        scope: ScopeId,
        span: Span,
    ) -> TypeId {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Function {
                generics,
                params,
                ret,
                ..
            } => {
                // Check type arg count
                let expected = generics.params.len();
                let found = type_args.len();

                if found != 0 && found != expected {
                    self.error(TypeError::WrongNumberOfTypeArgs {
                        expected,
                        found,
                        span,
                    });
                    return self.builtins.never;
                }

                // If no type args provided and function is generic,
                // we'll need inference later. For now, error.
                if found == 0 && expected != 0 {
                    self.error(TypeError::CannotInfer { span });
                    return self.builtins.never;
                }

                // Build function type
                // Need to resolve params and ret with substitutions if generic
                let fn_scope = ScopeId::Item(item_id);

                if !type_args.is_empty() {
                    let old_subs = std::mem::replace(&mut self.substitutions, type_args);
                    self.push_generic_params(generics);

                    let param_tys = params
                        .iter()
                        .map(|p| self.resolve_ast_type(p.ty, fn_scope))
                        .collect();

                    let ret =
                        ret.map_or(self.builtins.void, |r| self.resolve_ast_type(r, fn_scope));

                    self.pop_generic_params();
                    self.substitutions = old_subs;
                    self.intern_function(param_tys, ret)
                } else {
                    // Non-generic function
                    let param_tys = params
                        .iter()
                        .map(|p| self.resolve_ast_type(p.ty, fn_scope))
                        .collect();

                    let ret =
                        ret.map_or(self.builtins.void, |r| self.resolve_ast_type(r, fn_scope));

                    self.intern_function(param_tys, ret)
                }
            }

            ItemKind::Const { ty, value } => {
                if !type_args.is_empty() {
                    self.error(TypeError::TypeArgsOnConst { span });
                }

                match ty {
                    Some(ast_ty) => self.resolve_ast_type(*ast_ty, scope),
                    None => self.check_expr(*value, scope),
                }
            }

            ItemKind::Enum { .. }
            | ItemKind::Struct { .. }
            | ItemKind::Union { .. }
            | ItemKind::TypeAlias { .. }
            | ItemKind::Module { .. } => {
                self.error(TypeError::TypeUsedAsValue { span });
                self.builtins.never
            }

            ItemKind::Import(_) => {
                self.error(TypeError::TypeUsedAsValue { span });
                self.builtins.never
            }
        }
    }
}
