use crate::vm::instruction::load;

struct ObjectStruct {
    fields: Box<[u64]>,
}

struct ObjectString {
    value: String,
}

struct ObjectList {
    elems: Vec<u64>,
}

pub struct Heap {
    structs: Vec<ObjectStruct>,
    strings: Vec<ObjectString>,
    arrays: Vec<ObjectList>,
}

impl Heap {
    fn new() -> Self {
        Self {
            structs: vec![],
            strings: vec![],
            arrays: vec![],
        }
    }

    fn alloc_struct(&mut self, num_fields: usize) -> u64 {
        let ptr = self.structs.len();

        self.structs.push(ObjectStruct {
            fields: vec![0; num_fields].into(),
        });

        ptr as u64
    }

    fn set_struct_field(&mut self, ptr: u64, field: usize, value: u64) {
        self.structs[ptr as usize].fields[field] = value;
    }

    fn get_struct_field(&self, ptr: u64, field: usize) -> u64 {
        self.structs[ptr as usize].fields[field]
    }
}