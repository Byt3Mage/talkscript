pub struct TypeInfo {
    pub type_id: u16,
    pub name: String,
    pub num_fields: u16,
    pub ptr_fields: Box<[u16]>,
}

impl TypeInfo {
    pub const fn new(name: String, num_fields: u16, ptr_fields: Box<[u16]>, type_id: u16) -> Self {
        Self {
            type_id,
            name,
            num_fields,
            ptr_fields,
        }
    }
}

pub struct TypeRegistry {
    type_infos: Box<[TypeInfo]>,
}

impl TypeRegistry {
    #[inline(always)]
    pub fn get_info(&self, type_id: u16) -> &TypeInfo {
        &self.type_infos[type_id as usize]
    }
}
