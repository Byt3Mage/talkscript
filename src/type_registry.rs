use std::any::type_name;

use ahash::AHashMap;

use crate::arena::Arena;

slotmap::new_key_type! {
    pub struct TypeId;
}

/// Complete type information
pub struct TypeInfo {
    pub name: String,
    /// What kind of type this is
    pub kind: Type,
    /// Size in VM words (register slots)
    ///
    /// None => type is unsized. e.g [T]
    pub size: Option<usize>,
}

pub enum Type {
    // Primitives (all 1 word except Void/Never = 0)
    Int,
    Float,
    Bool,
    Str,  //Unsized raw string data, only available via pointer
    CStr, //Interned constant string, size - 1 word

    Void,
    Never,
    Any,

    // Compound types
    Optional(TypeId),                              // T? - niche for bool and @T
    Array { element: TypeId, length: usize },      //[T; N] - fixed size
    DynArray(TypeId),                              //Unsized, can only be stored on the heap
    Function { params: Vec<TypeId>, ret: TypeId }, // fn(T1, T2) -> R - 1 word
    Pointer { pointee: TypeId, mutable: bool },    // @T - 1 word GC pointer

    // User defined types
    Struct(StructInfo),
    Enum(EnumInfo),
}

/// Struct type metadata
pub struct StructInfo {
    pub fields: Vec<FieldInfo>,
}

/// Field within a struct
pub struct FieldInfo {
    pub name: String,
    pub ty: TypeId,
    /// Offset in VM words from struct start
    pub offset: usize,
}

/// Enum type metadata
pub struct EnumInfo {
    pub variants: Vec<VariantInfo>,
}

/// A single enum variant
pub struct VariantInfo {
    pub name: String,
    pub tag: usize,
    pub payload: VariantPayload,
}

pub enum VariantPayload {
    None,
    Struct(Vec<FieldInfo>),
}

#[derive(Hash, Eq, PartialEq, Clone, Copy)]
enum Primitive {
    Int,
    Float,
    Bool,
    Str,
    CStr,
    Void,
    Never,
    Any,
}

/// For type interning (deduplication)
#[derive(Hash, Eq, PartialEq, Clone)]
enum TypeKey {
    Primitive(Primitive),
    Optional(TypeId),
    Array(TypeId, usize),
    DynArray(TypeId),
    Function(Vec<TypeId>, TypeId),
    Pointer(TypeId, bool),
    User(String), // Enums/Structs are nominal (identified by name)
}

/// The type registry - central type system
pub struct TypeRegistry {
    // All types stored here
    types: Arena<TypeId, TypeInfo>,

    // Type interning - avoid duplicates
    type_cache: AHashMap<TypeKey, TypeId>,

    // Quick access to built-in types
    pub int_type: TypeId,
    pub float_type: TypeId,
    pub bool_type: TypeId,
    pub str_type: TypeId,
    pub cstr_type: TypeId,
    pub void_type: TypeId,
    pub never_type: TypeId,
    pub any_type: TypeId,

    // User-defined types by name
    user_types: AHashMap<String, TypeId>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut types = Arena::with_key();
        let mut type_cache = AHashMap::new();

        // Helper to insert a primitive type
        let mut insert_prim = |name: &str, prim: Primitive, kind: Type, size: Option<usize>| {
            let type_id = types.insert(TypeInfo {
                name: name.to_string(),
                kind,
                size,
            });
            type_cache.insert(TypeKey::Primitive(prim), type_id);
            type_id
        };

        let int_type = insert_prim("int", Primitive::Int, Type::Int, Some(1));
        let float_type = insert_prim("float", Primitive::Float, Type::Float, Some(1));
        let bool_type = insert_prim("bool", Primitive::Bool, Type::Bool, Some(1));
        let str_type = insert_prim("str", Primitive::Str, Type::Str, None);
        let cstr_type = insert_prim("cstr", Primitive::CStr, Type::CStr, Some(1));
        let any_type = insert_prim("any", Primitive::Any, Type::Any, Some(1));
        let void_type = insert_prim("void", Primitive::Void, Type::Void, Some(0));
        let never_type = insert_prim("never", Primitive::Never, Type::Never, Some(0));

        Self {
            types,
            type_cache,
            int_type,
            float_type,
            bool_type,
            str_type,
            cstr_type,
            void_type,
            never_type,
            any_type,
            user_types: AHashMap::new(),
        }
    }

    /// Intern a type - returns existing TypeId if already cached
    fn new_type(&mut self, key: TypeKey, name: String, kind: Type, size: Option<usize>) -> TypeId {
        let type_id = self.types.insert(TypeInfo { name, kind, size });
        self.type_cache.insert(key, type_id);
        type_id
    }

    /// Create an optional type T?
    pub fn make_optional(&mut self, inner: TypeId) -> Result<TypeId> {
        let key = TypeKey::Optional(inner);

        if let Some(&id) = self.type_cache.get(&key) {
            return Ok(id);
        }

        let inner_ty = &self.types[inner];
        let name = format!("{}?", inner_ty.name);
        let size = match inner_ty.kind {
            Type::Bool => Some(1),           // Niche: use 2+ for None
            Type::Pointer { .. } => Some(1), // Niche: null = None
            _ => match inner_ty.size {
                Some(size) => Some(size + 1), // Regular: discriminant + value
                None => return Err(Error::UnsizedOptional { ty: name }),
            },
        };

        let type_id = self.new_type(key, name, Type::Optional(inner), size);
        Ok(type_id)
    }

    /// Create a pointer type @T
    pub fn make_pointer(&mut self, pointee: TypeId, mutable: bool) -> TypeId {
        let key = TypeKey::Pointer(pointee, mutable);

        if let Some(&id) = self.type_cache.get(&key) {
            return id;
        }

        let name = if mutable {
            format!("@{}", &self.types[pointee].name)
        } else {
            format!("@mut {}", &self.types[pointee].name)
        };

        self.new_type(key, name, Type::Pointer { pointee, mutable }, Some(1))
    }

    /// Create a fixed-size array [T; N]
    pub fn make_array(&mut self, element: TypeId, length: usize) -> Result<TypeId> {
        let key = TypeKey::Array(element, length);

        if let Some(&id) = self.type_cache.get(&key) {
            return Ok(id);
        }

        let elem_ty = &self.types[element];

        let size = match elem_ty.size {
            Some(size) => Some(size * length),
            None => {
                return Err(Error::UnsizedElement {
                    ty: elem_ty.name.clone(),
                });
            }
        };

        let name = format!("[{}; {}]", elem_ty.name, length);
        let type_id = self.new_type(key, name, Type::Array { element, length }, size);

        Ok(type_id)
    }

    /// Create a dynamic array [T] (unsized)
    pub fn make_dyn_array(&mut self, element: TypeId) -> TypeId {
        let key = TypeKey::DynArray(element);

        if let Some(&id) = self.type_cache.get(&key) {
            return id;
        }

        let name = format!("[{}]", &self.types[element].name);
        self.new_type(key, name, Type::DynArray(element), None)
    }

    /// Create a function type fn(T1, T2, ...) -> R
    pub fn make_function(&mut self, params: Vec<TypeId>, ret: TypeId) -> Result<TypeId> {
        let key = TypeKey::Function(params.clone(), ret);

        if let Some(&id) = self.type_cache.get(&key) {
            return Ok(id);
        }

        let ret_ty = &self.types[ret];

        if ret_ty.size.is_none() {
            return Err(Error::UnsizedReturn {
                ty: ret_ty.name.clone(),
            });
        }

        let param_names: Result<Vec<&str>> = params
            .iter()
            .map(|&p| {
                let ty = &self.types[p];
                match ty.size {
                    Some(_) => Ok(ty.name.as_str()),
                    None => Err(Error::UnsizedParam {
                        ty: ty.name.clone(),
                    }),
                }
            })
            .collect();

        let name = format!("fn({}) -> {}", param_names?.join(", "), ret_ty.name);
        let type_id = self.new_type(key, name, Type::Function { params, ret }, Some(1));

        Ok(type_id)
    }

    pub fn register_struct(
        &mut self,
        name: String,
        fields: Vec<(String, TypeId)>,
    ) -> Result<TypeId> {
        // Check for duplicate registration
        if self.user_types.contains_key(&name) {
            return Err(Error::DuplicateType { name });
        }

        // Compute field offsets and total size
        let mut field_infos = Vec::new();
        let mut offset = 0;

        for field in fields {
            let field_ty = &self.types[field.1];
            let field_size = match field_ty.size {
                Some(size) => size,
                None => {
                    return Err(Error::UnsizedStructField {
                        struct_name: name.clone(),
                        field_name: field.0.clone(),
                        field_type: field_ty.name.clone(),
                    });
                }
            };

            field_infos.push(FieldInfo {
                name: field.0,
                ty: field.1,
                offset,
            });

            offset += field_size;
        }

        let type_id = self.new_type(
            TypeKey::User(name.clone()),
            name.clone(),
            Type::Struct(StructInfo {
                fields: field_infos,
            }),
            Some(offset),
        );

        self.user_types.insert(name, type_id);

        Ok(type_id)
    }
    /// Register an enum type
    pub fn register_enum(
        &mut self,
        name: String,
        variants: Vec<(String, VariantPayload)>, // (variant_name, payload)
    ) -> Result<TypeId> {
        // Build variant infos with tags
        let mut variant_infos = Vec::new();
        let mut max_payload_size = 0;

        for (tag, (variant_name, payload)) in variants.into_iter().enumerate() {
            let payload_size = match &payload {
                VariantPayload::None => 0,
                VariantPayload::Struct(fields) => {
                    let mut size = 0;
                    for field in fields {
                        let field_ty_info = &self.types[field.ty];
                        let field_size =
                            field_ty_info.size.ok_or_else(|| Error::UnsizedVariant {
                                enum_name: name.clone(),
                                variant_name: variant_name.clone(),
                                field_name: field.name.clone(),
                                field_type: field_ty_info.name.clone(),
                            })?;
                        size += field_size;
                    }
                    size
                }
            };

            max_payload_size = max_payload_size.max(payload_size);

            variant_infos.push(VariantInfo {
                name: variant_name,
                tag,
                payload,
            });
        }

        // Enum size = discriminant (1 word) + largest payload
        let total_size = 1 + max_payload_size;

        let type_id = self.new_type(
            TypeKey::User(name.clone()),
            name.clone(),
            Type::Enum(EnumInfo {
                variants: variant_infos,
            }),
            Some(total_size),
        );

        self.user_types.insert(name.clone(), type_id);

        Ok(type_id)
    }
}

pub enum Error {
    UnsizedElement {
        ty: String,
    },
    UnsizedOptional {
        ty: String,
    },
    UnsizedReturn {
        ty: String,
    },
    UnsizedParam {
        ty: String,
    },
    UnsizedStructField {
        struct_name: String,
        field_name: String,
        field_type: String,
    },
    UnsizedVariant {
        enum_name: String,
        variant_name: String,
        field_name: String,
        field_type: String,
    },
    DuplicateType {
        name: String,
    },
}

type Result<T> = std::result::Result<T, Error>;
