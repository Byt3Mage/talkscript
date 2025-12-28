use std::collections::hash_map::Entry;

use ahash::{AHashMap, AHashSet};

use crate::{
    arena::{Ident, Interner},
    compiler::{
        ast::{
            AstArena, AstTypeId, AstTypeKind, ExprId, Field, ItemId, ItemKind, Param, PatternId,
            PatternKind, Variant,
        },
        name_resolver::SymbolTable,
        tokens::Span,
        type_info::{
            EnumInfo, FieldInfo, StructInfo, TypeArena, TypeId, TypeValue, UnionInfo, VariantInfo,
        },
    },
};

pub struct BuiltinTypes {
    pub int: TypeId,
    pub uint: TypeId,
    pub float: TypeId,
    pub bool_: TypeId,
    pub char_: TypeId,
    pub str_: TypeId,
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
            bool_: types.insert(TypeValue::Bool),
            char_: types.insert(TypeValue::Char),
            str_: types.insert(TypeValue::Str),
            cstr: types.insert(TypeValue::Cstr),
            void: types.insert(TypeValue::Void),
            never: types.insert(TypeValue::Never),
        }
    }
}

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
    mono_cache: AHashMap<(ItemId, Vec<TypeId>), TypeId>,

    // Currently resolving (for cycle detection)
    resolving: AHashSet<(ItemId, Vec<TypeId>)>,

    // Scopes for tracking variable types
    local_scopes: Vec<AHashMap<Ident, TypeId>>,

    // Type substitutions for current generic context
    // Maps generic param index -> concrete TypeId
    substitutions: Vec<TypeId>,

    // Results
    expr_types: AHashMap<ExprId, TypeId>,
    pattern_types: AHashMap<PatternId, TypeId>,

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
            substitutions: Vec::new(),
            expr_types: AHashMap::new(),
            pattern_types: AHashMap::new(),
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

    pub fn check_module(&mut self, items: &[ItemId]) {
        // First pass: register type declarations (creates placeholders)
        for &item_id in items {
            self.register_type_decl(item_id);
        }

        // Second pass: resolve type bodies and check expressions
        for &item_id in items {
            self.check_item(item_id);
        }
    }

    fn register_type_decl(&mut self, item_id: ItemId) {
        let item = &self.ast.items[item_id];

        // Generics are instantiated on demand
        match &item.kind {
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

            // No type declarations
            ItemKind::Function { .. }
            | ItemKind::Const { .. }
            | ItemKind::Impl { .. }
            | ItemKind::Import(_) => {}
        }
    }

    // ========================================================================
    // Second Pass: Check Items
    // ========================================================================

    fn check_item(&mut self, item_id: ItemId) {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Struct { generics, fields } => {
                if generics.params.is_empty() {
                    self.resolve_struct(item_id, item.name, fields);
                }
            }

            ItemKind::Union { generics, fields } => {
                if generics.params.is_empty() {
                    self.resolve_union(item_id, item.name, fields);
                }
            }

            ItemKind::Enum { variants } => {
                self.resolve_enum(item_id, item.name, variants);
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
                self.check_const(*ty, *value);
            }

            ItemKind::TypeAlias { generics, ty } => {
                if generics.params.is_empty() {
                    self.resolve_type_alias(item_id, *ty);
                }
            }

            ItemKind::Impl {
                generics,
                self_ty,
                items,
            } => {
                self.check_impl(generics, *self_ty, items);
            }

            ItemKind::Module { items } => {
                self.check_module(items);
            }

            ItemKind::Import(_) => {}
        }
    }

    fn resolve_struct(&mut self, item_id: ItemId, name: Ident, fields: &[Field]) {
        // Get the placeholder we created in first pass
        let type_id = self.type_decls[&item_id];

        // Resolve all field types
        let field_infos: Vec<FieldInfo> = fields
            .iter()
            .map(|f| FieldInfo {
                name: f.name,
                ty: self.resolve_ast_type(f.ty),
            })
            .collect();

        // Replace placeholder with actual struct
        *self.types.get_mut(type_id) = TypeValue::Struct(StructInfo {
            name,
            fields: field_infos,
            methods: Vec::new(),
        });
    }

    fn resolve_union(&mut self, item_id: ItemId, name: Ident, fields: &[Field]) {
        let type_id = self.type_decls[&item_id];

        let field_infos: Vec<FieldInfo> = fields
            .iter()
            .map(|f| FieldInfo {
                name: f.name,
                ty: self.resolve_ast_type(f.ty),
            })
            .collect();

        *self.types.get_mut(type_id) = TypeValue::Union(UnionInfo {
            name,
            fields: field_infos,
            methods: Vec::new(),
        });
    }

    fn resolve_enum(&mut self, item_id: ItemId, name: Ident, variants: &[Variant]) {
        let type_id = self.type_decls[&item_id];

        let mut variant_infos = Vec::new();
        let mut seen_values: AHashMap<i64, Span> = AHashMap::new();
        let mut next_value: i64 = 0;

        for variant in variants {
            let value = if let Some(expr_id) = variant.value {
                // Evaluate constant expression
                match self.eval_const_expr(expr_id) {
                    Some(v) => {
                        next_value = v + 1;
                        v
                    }
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
                Entry::Occupied(entry) => {
                    self.error(TypeError::DuplicateEnumValue {
                        name: variant.name,
                        value,
                        first_def: *entry.get(),
                        dupe_def: variant.span,
                    });
                }
                Entry::Vacant(entry) => {
                    seen_values.insert(value, variant.span);
                }
            }

            variant_infos.push(VariantInfo {
                name: variant.name,
                value,
            });
        }

        *self.types.get_mut(type_id) = TypeValue::Enum(EnumInfo {
            name,
            variants: variant_infos,
            methods: Vec::new(),
        });
    }

    fn resolve_type_alias(&mut self, item_id: ItemId, ty: AstTypeId) {
        let type_id = self.type_decls[&item_id];
        let resolved = self.resolve_ast_type(ty);

        // Type alias is just an indirection - copy the resolved type
        *self.types.get_mut(type_id) = self.types.get(resolved).clone();
    }

    fn check_const(&mut self, ty: Option<AstTypeId>, value: ExprId) {
        let value_ty = self.check_expr(value);

        if let Some(ast_ty) = ty {
            let declared_ty = self.resolve_ast_type(ast_ty);
            self.expect_type(value_ty, declared_ty, self.ast.exprs[value].span);
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

        for param in params {
            let param_ty = self.resolve_ast_type(param.ty);
            self.bind_pattern(param.pattern, param_ty);
        }

        let ret_ty = ret.map_or(self.builtins.void, |r| self.resolve_ast_type(r));
        let body_ty = self.check_expr(body);

        self.expect_type(body_ty, ret_ty, self.ast.exprs[body].span);

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
                    TypeValue::Struct(info) => &info.fields,
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
    fn resolve_ast_type(&mut self, ast_type_id: AstTypeId) -> TypeId {
        let ast_type = &self.ast.types[ast_type_id];

        match &ast_type.kind {
            AstTypeKind::Path(path) => self.resolve_type_path(path, ast_type.span),

            AstTypeKind::Pointer { pointee, mutable } => {
                let pointee_ty = self.resolve_ast_type(*pointee);
                self.types.insert(TypeValue::Pointer {
                    pointee: pointee_ty,
                    mutable: *mutable,
                })
            }

            AstTypeKind::Optional(inner) => {
                let inner_ty = self.resolve_ast_type(*inner);
                self.types.insert(TypeValue::Optional(inner_ty))
            }

            AstTypeKind::Array { elem, size } => {
                let elem_ty = self.resolve_ast_type(*elem);
                let len = match self.eval_const_expr(*size) {
                    Some(n) if n >= 0 => n as usize,
                    _ => {
                        self.error(TypeError::InvalidArrayLength {
                            span: ast_type.span,
                        });
                        0
                    }
                };
                self.types.insert(TypeValue::Array { elem: elem_ty, len })
            }

            AstTypeKind::Slice(elem) => {
                let elem_ty = self.resolve_ast_type(*elem);
                self.types.insert(TypeValue::Slice(elem_ty))
            }

            AstTypeKind::Tuple(elems) => {
                let elem_tys: Vec<TypeId> =
                    elems.iter().map(|&e| self.resolve_ast_type(e)).collect();
                self.types.insert(TypeValue::Tuple(elem_tys))
            }

            AstTypeKind::Function { params, ret } => {
                let param_tys: Vec<TypeId> =
                    params.iter().map(|&p| self.resolve_ast_type(p)).collect();
                let ret_ty = self.resolve_ast_type(*ret);
                self.types.insert(TypeValue::Function {
                    params: param_tys,
                    ret: ret_ty,
                })
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
                let field_infos: Vec<FieldInfo> = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name,
                        ty: self.resolve_ast_type(f.ty),
                    })
                    .collect();
                self.types.insert(TypeValue::Struct(StructInfo {
                    name: self.interner.get_or_intern_static("<anon>"),
                    fields: field_infos,
                    methods: Vec::new(),
                }))
            }

            AstTypeKind::AnonUnion(fields) => {
                let field_infos: Vec<FieldInfo> = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name,
                        ty: self.resolve_ast_type(f.ty),
                    })
                    .collect();
                self.types.insert(TypeValue::Union(UnionInfo {
                    name: self.interner.get_or_intern_static("<anon>"),
                    fields: field_infos,
                    methods: Vec::new(),
                }))
            }

            AstTypeKind::AnonEnum(variants) => {
                // Same logic as resolve_enum but inline
                let mut variant_infos = Vec::new();
                let mut next_value: i64 = 0;

                for variant in variants {
                    let value = if let Some(expr_id) = variant.value {
                        match self.eval_const_expr(expr_id) {
                            Some(v) => {
                                next_value = v + 1;
                                v
                            }
                            None => {
                                next_value += 1;
                                next_value - 1
                            }
                        }
                    } else {
                        let v = next_value;
                        next_value += 1;
                        v
                    };

                    variant_infos.push(VariantInfo {
                        name: variant.name,
                        value,
                    });
                }

                self.types.insert(TypeValue::Enum(EnumInfo {
                    name: self.interner.get_or_intern_static("<anon>"),
                    variants: variant_infos,
                    methods: Vec::new(),
                }))
            }
        }
    }
}
