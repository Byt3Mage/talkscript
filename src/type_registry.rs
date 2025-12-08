use std::sync::Arc;

pub type TypeId = u16;

#[derive(Debug)]
pub struct StructInfo {
    pub name: String,
    pub num_fields: u16,
    pub ptr_fields: Arc<[u16]>,
}

#[derive(Debug)]
pub struct ArrayInfo {
    pub elem_type_id: TypeId,
    pub elem_is_ptr: bool,
}

pub struct MapInfo {
    pub key_type_id: TypeId,
    pub val_type_id: TypeId,
    pub key_is_ptr: bool,
    pub val_is_ptr: bool,
}

#[derive(Debug)]
pub enum TypeInfo {
    Struct(StructInfo),
    Array(ArrayInfo),
}

pub struct TypeRegistry {
    type_infos: Box<[TypeInfo]>,
}

impl TypeRegistry {
    pub fn new(type_infos: impl Into<Box<[TypeInfo]>>) -> Self {
        Self {
            type_infos: type_infos.into(),
        }
    }

    #[inline(always)]
    pub fn get_info(&self, type_id: u16) -> &TypeInfo {
        &self.type_infos[type_id as usize]
    }
}
