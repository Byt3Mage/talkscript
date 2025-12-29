use std::{fmt::Debug, slice::Iter};

use crate::vm::{
    Task,
    heap::{GCHeader, GCPtr},
    object::private::Sealed,
};

macro_rules! value {
    ($($field: ident : $ty: path),* $(,)?) => {
        mod private {
            pub trait Sealed {}
        }

        pub trait AsValue: Sealed + Sized {
            const TYPE: ValueType;
            fn from_value(value: Value) -> Self;
            fn into_value(self: Self) -> Value;
            fn try_from_value(value: Value) -> Option<Self>;
        }

        #[repr(u8)]
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub enum ValueType {
            $($field),*
        }

        #[repr(C)]
        #[derive(Clone, Copy)]
        union ValueData {
            $($field: $ty),*
        }

        #[derive(Clone, Copy)]
        pub struct Value {
            data: ValueData,
            ty: ValueType,
        }

        const _: () = assert!(align_of::<Value>() == align_of::<u64>());

        impl Value {
            #[inline(always)]
            pub fn zero() -> Self {
                unsafe { std::mem::zeroed::<Self>() }
            }

            #[inline(always)]
            pub fn get<T: AsValue>(self) -> T {
                T::from_value(self)
            }

            #[inline(always)]
            pub fn try_get<T: AsValue>(self) -> Option<T> {
                T::try_from_value(self)
            }

            #[inline(always)]
            pub fn set(&mut self, v: impl AsValue) {
                *self = v.into_value();
            }
        }

        $(
            impl Sealed for $ty {}
            impl AsValue for $ty {
                const TYPE: ValueType = ValueType::$field;

                fn from_value(value: Value) -> Self {
                    unsafe { value.data.$field }
                }

                fn into_value(self: Self) -> Value {
                    Value {
                        data: ValueData { $field: self },
                        ty: Self::TYPE,
                    }
                }

                fn try_from_value(value: Value) -> Option<Self> {
                    (value.ty == Self::TYPE).then_some(unsafe { value.data.$field })
                }
            }
        )*
    };
}

value! {
    Ptr: GCPtr,
    Int: i64,
    Uint: u64,
    Bool: bool,
    Float: f64,
    // Niche values that can live in a single register
    OptPtr: Option<GCPtr>,
    OptBool: Option<bool>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObjType {
    /// Used to indicate that the object block is free
    //Null,
    Buffer,
    DynBuffer,
    String,
    Task,
}

#[repr(C)]
pub struct GCBuffer {
    header: GCHeader,
    pub gc_list: Option<GCPtr>,
    size: usize,
    data: [Value; 0],
}

impl GCBuffer {
    const _ASSERT: () = assert!(align_of::<Self>() == align_of::<Value>());

    pub fn new(header: GCHeader, size: usize) -> Self {
        Self {
            header,
            gc_list: None,
            size,
            data: [],
        }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.size
    }

    #[inline(always)]
    pub fn data(&self) -> *mut Value {
        self.data.as_ptr().cast_mut()
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.data(), self.size as usize) }
    }

    #[inline(always)]
    pub fn as_slice_mut(&mut self) -> &mut [Value] {
        unsafe { std::slice::from_raw_parts_mut(self.data(), self.size) }
    }

    pub fn iter(&'_ self) -> Iter<'_, Value> {
        self.as_slice().iter()
    }
}

#[repr(C)]
pub(super) struct GCDynBuffer {
    header: GCHeader,
    pub gc_list: Option<GCPtr>,
    data: Vec<Value>,
}

impl GCDynBuffer {
    pub fn new(header: GCHeader) -> Self {
        Self {
            header,
            gc_list: None,
            data: vec![],
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
pub(super) struct GCString {
    header: GCHeader,
    data: String,
}

impl GCString {
    pub fn new(header: GCHeader) -> Self {
        Self {
            header,
            data: String::new(),
        }
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
pub(super) struct GCTask {
    pub(super) header: GCHeader,
    pub(super) data: Task,
}

impl GCTask {
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
