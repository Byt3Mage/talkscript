use crate::{
    arena::{Arena, Ident},
    compiler::ast::ItemId,
};

slotmap::new_key_type! {
    pub struct TypeId;
}

#[derive(Debug, Clone)]
pub enum TypeValue {
    // Primitives
    Int,
    Uint,
    Float,
    Bool,
    Char,
    Str,
    Cstr,
    Void,
    Never,

    // Compound
    Pointer { pointee: TypeId, mutable: bool },
    Optional(TypeId),
    Array { elem: TypeId, len: usize },
    Slice(TypeId),
    Tuple(Vec<TypeId>),

    // User-defined
    Struct(StructInfo),
    Union(UnionInfo),
    Enum(EnumInfo),

    // Functions
    Function { params: Vec<TypeId>, ret: TypeId },

    // Placeholder for recursive types
    Incomplete,
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: Ident,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<ItemId>,
}

#[derive(Debug, Clone)]
pub struct UnionInfo {
    pub name: Ident,
    pub fields: Vec<FieldInfo>, // each field is a variant
    pub methods: Vec<ItemId>,
}

#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: Ident,
    pub variants: Vec<VariantInfo>,
    pub methods: Vec<ItemId>,
}

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: Ident,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub name: Ident,
    pub value: i64, // resolved discriminant value
}

pub struct TypeArena {
    types: Arena<TypeId, TypeValue>,
}

impl TypeArena {
    pub fn new() -> Self {
        Self {
            types: slotmap::SlotMap::with_key(),
        }
    }

    pub fn insert(&mut self, ty: TypeValue) -> TypeId {
        self.types.insert(ty)
    }

    pub fn get(&self, id: TypeId) -> &TypeValue {
        &self.types[id]
    }

    pub fn get_mut(&mut self, id: TypeId) -> &mut TypeValue {
        &mut self.types[id]
    }
}
