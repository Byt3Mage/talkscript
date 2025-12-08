use std::{fmt::Debug, slice::Iter};

use crate::vm::{
    Task,
    heap::{GCHeader, GCPtr},
};

macro_rules! value {
    ($($field: ident : $ty: ty),*$(,)?) => {
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub union Value {
            $($field: $ty),*
        }

        // ASSERTIONS
        $( const _: () = assert!(!std::mem::needs_drop::<$ty>()); )*
        const _: () = assert!(size_of::<Value>() == size_of::<u64>() && align_of::<Value>() == align_of::<u64>());

        impl Value {
            #[inline(always)]
            pub fn zero() -> Self {
                unsafe { std::mem::zeroed::<Self>() }
            }

            #[inline(always)]
            pub fn get<T: From<Self>>(self) -> T {
                self.into()
            }

            #[inline(always)]
            pub fn set(&mut self, v: impl Into<Self>) {
                *self = v.into();
            }
        }

        impl Debug for Value {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(format_args!("{}", unsafe { std::mem::transmute::<_, u64>(*self) }))
            }
        }

        $(
            impl From<$ty> for Value {
                fn from(value: $ty) -> Self {
                    Self { $field: value }
                }
            }

            impl From<Value> for $ty {
                fn from(value: Value) -> Self {
                    unsafe { value.$field }
                }
            }
        )*
    };
}

value! {
    ptr: GCPtr,
    int: i64,
    uint: u64,
    bool: bool,
    float: f64,
    // Niche values that can live in a single register
    opt_bol: Option<bool>,
    opt_ptr: Option<GCPtr>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObjType {
    /// Used to indicate that the object block is free
    //Null,
    Array,
    List,
    Struct,
    String,
    Task,

    /// Array with pointer elements
    PtrArray,
    /// List with pointer elements
    PtrList,
    /// Structs with pointer field(s)
    PtrStruct,
}

#[repr(C)]
pub struct ObjArray {
    header: GCHeader,
    len: u64,
    pub gc_list: Option<GCPtr>,
    // Array data follows after (inline allocation)
    data: [Value; 0],
}

impl ObjArray {
    const _ASSERT: () = assert!(align_of::<Self>() == align_of::<Value>());

    pub fn new(header: GCHeader, len: u64) -> Self {
        Self {
            header,
            len,
            gc_list: None,
            data: [],
        }
    }

    #[inline(always)]
    pub fn len(&self) -> u64 {
        self.len
    }

    #[inline(always)]
    pub fn data(&self) -> *mut Value {
        self.data.as_ptr().cast_mut()
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.data(), self.len as usize) }
    }

    #[inline(always)]
    pub fn as_slice_mut(&mut self) -> &mut [Value] {
        unsafe { std::slice::from_raw_parts_mut(self.data(), self.len as usize) }
    }

    pub fn iter(&'_ self) -> Iter<'_, Value> {
        self.as_slice().iter()
    }
}

#[repr(C)]
pub(super) struct ObjList {
    header: GCHeader,
    data: Vec<Value>,
    pub gc_list: Option<GCPtr>,
}

impl ObjList {
    pub fn new(header: GCHeader, elems: &[Value]) -> Self {
        Self {
            header,
            data: elems.iter().copied().collect(),
            gc_list: None,
        }
    }

    #[inline(always)]
    pub(super) fn get(&self) -> &Vec<Value> {
        &self.data
    }

    #[inline(always)]
    pub(super) fn get_mut(&mut self) -> &mut Vec<Value> {
        &mut self.data
    }

    #[inline(always)]
    pub fn iter(&self) -> Iter<'_, Value> {
        self.data.iter()
    }
}

#[repr(C)]
pub(super) struct ObjStruct {
    header: GCHeader,
    size_in_words: u64,
    ptr_mask: u64,
    pub gc_list: Option<GCPtr>,
    data: [Value; 0],
}

impl ObjStruct {
    const _ASSERT: () = assert!(align_of::<Self>() == align_of::<Value>());

    pub fn new(header: GCHeader, size_in_words: u64, ptr_mask: u64) -> Self {
        Self {
            header,
            size_in_words,
            ptr_mask,
            gc_list: None,
            data: [],
        }
    }

    #[inline(always)]
    pub(super) fn fields_ptr(&self) -> *mut Value {
        self.data.as_ptr().cast_mut()
    }

    #[inline(always)]
    pub(super) fn has_ptr_at(&self, index: usize) -> bool {
        debug_assert!(index < 64, "structs can't have more than 64 fields");
        (self.ptr_mask & (1 << index)) != 0
    }

    #[inline(always)]
    pub fn fields(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.fields_ptr(), self.size_in_words as usize) }
    }
}

#[repr(C)]
pub(super) struct ObjString {
    header: GCHeader,
    data: String,
}

impl ObjString {
    pub fn new(header: GCHeader, data: String) -> Self {
        Self { header, data }
    }

    #[inline(always)]
    pub(super) fn get(&self) -> &String {
        &self.data
    }

    #[inline(always)]
    pub(super) fn get_mut(&mut self) -> &mut String {
        &mut self.data
    }
}

#[repr(C)]
pub(super) struct ObjTask {
    pub(super) header: GCHeader,
    pub(super) data: Task,
}

impl ObjTask {
    pub fn new(header: GCHeader, data: Task) -> Self {
        Self { header, data }
    }

    #[inline(always)]
    pub(super) fn get(&self) -> &Task {
        &self.data
    }

    #[inline(always)]
    pub(super) fn get_mut(&mut self) -> &mut Task {
        &mut self.data
    }
}
