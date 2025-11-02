use crate::vm::vm_types::{Boolean, Float, Integer, Pointer};

pub type StackValue = u64;

pub struct Stack {
    data: Vec<StackValue>,  // underlying storage
    sp: usize,              // stack pointer (index into data)
    frame_ptrs: Vec<usize>, // call frame boundaries
}

impl Stack {
    pub fn new() -> Self {
        Self {
            data: vec![],
            sp: 0,
            frame_ptrs: vec![],
        }
    }

    pub(crate) fn push(&mut self, value: StackValue) {
        if self.sp == self.data.len() {
            self.data.push(value);
        } else {
            self.data[self.sp] = value;
        }
        self.sp += 1;
    }

    pub(crate) fn pop(&mut self) -> StackValue {
        self.sp -= 1;
        self.data[self.sp]
    }

    pub(crate) fn top(&self) -> StackValue {
        self.data[self.sp - 1]
    }
}
