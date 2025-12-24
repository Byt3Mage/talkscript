use ahash::AHashMap;

use crate::{
    arena::{Arena, Ident},
    compiler::canon_ast::{Decl, DeclId},
};

slotmap::new_key_type! {
    pub struct TypeId;
}

pub struct TypeArena {
    types: Arena<TypeId, TypeValue>,
}

impl TypeArena {
    pub fn new() -> Self {
        Self {
            types: Arena::with_key(),
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

#[derive(Debug, Clone, PartialEq)]
pub enum TypeValue {
    Type,
    Int,
    Uint,
    Float,
    Bool,
    Cstr,
    Str,
    Char,
    Void,
    Never,
    Pointer { pointee: TypeId, mutable: bool },
    Optional(TypeId),
    Array { element: TypeId, len: i64 },
    Slice(TypeId),
    Tuple { fields: Vec<TypeId> },
    Struct(StructInfo),
    Enum(EnumInfo),
    Union(UnionInfo),
    Function(FunctionInfo),
    Module(ModuleInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo {
    pub name: Ident,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Declaration {
    pub name: Ident,
    pub id: DeclId,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct StructInfo {
    pub fields: Vec<FieldInfo>,
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct UnionInfo {
    pub fields: Vec<FieldInfo>,
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariantInfo {
    pub name: Ident,
    pub value: i64,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct EnumInfo {
    pub variants: Vec<VariantInfo>,
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParamInfo {
    pub name: Option<Ident>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    pub params: Vec<ParamInfo>,
    pub ret: TypeId,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ModuleInfo {
    pub decls: Vec<Declaration>,
}

impl StructInfo {
    pub fn find_decl(&self, name: Ident) -> Option<&Declaration> {
        self.decls.iter().find(|d| d.name == name)
    }
}

impl UnionInfo {
    pub fn find_decl(&self, name: Ident) -> Option<&Declaration> {
        self.decls.iter().find(|d| d.name == name)
    }
}

impl EnumInfo {
    pub fn find_decl(&self, name: Ident) -> Option<&Declaration> {
        self.decls.iter().find(|d| d.name == name)
    }
}

impl ModuleInfo {
    pub fn find_decl(&self, name: Ident) -> Option<&Declaration> {
        self.decls.iter().find(|d| d.name == name)
    }
}
