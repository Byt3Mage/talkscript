use std::{alloc::Layout, ptr::NonNull};

use simple_ternary::tnr;

use crate::vm::{
    allocator::{Allocator, BumpAllocator},
    async_runtime::Task,
    object::*,
};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GCColor {
    Gray = 0b000,
    White0 = 0b001,
    White1 = 0b010,
    Black = 0b100,
}

impl GCColor {
    #[inline]
    pub const fn is_white(self) -> bool {
        matches!(self, GCColor::White0 | GCColor::White1)
    }

    #[inline]
    pub const fn is_black(self) -> bool {
        matches!(self, GCColor::Black)
    }

    #[inline]
    pub const fn is_gray(self) -> bool {
        matches!(self, GCColor::Gray)
    }

    #[inline]
    pub const fn other_white(self) -> GCColor {
        match self {
            GCColor::White0 => GCColor::White1,
            GCColor::White1 => GCColor::White0,
            _ => panic!("other_white called on non-white color"),
        }
    }
}

pub struct GCHeader {
    color: GCColor,
    obj_type: ObjType,
    next: Option<GCPtr>,
    layout: Layout,
}

impl GCHeader {
    #[inline(always)]
    pub fn ty(&self) -> ObjType {
        self.obj_type
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct GCPtr(NonNull<GCHeader>);

impl GCPtr {
    #[inline(always)]
    pub(super) fn new(ptr: NonNull<u8>) -> Self {
        Self(ptr.cast())
    }

    #[inline(always)]
    pub(super) fn as_ptr<T>(self) -> NonNull<T> {
        self.0.cast()
    }

    #[inline(always)]
    pub(super) fn hdr(&self) -> &GCHeader {
        unsafe { self.0.as_ref() }
    }

    #[inline(always)]
    pub(super) fn hdr_mut(&mut self) -> &mut GCHeader {
        unsafe { self.0.as_mut() }
    }

    #[inline(always)]
    pub(super) fn ty(&self) -> ObjType {
        self.hdr().ty()
    }

    pub(super) fn as_ref<T>(&self) -> &T {
        unsafe { self.0.cast().as_ref() }
    }

    pub(super) fn as_mut<T>(&mut self) -> &mut T {
        unsafe { self.0.cast().as_mut() }
    }
}

const MAX_THRESHOLD: usize = 1024 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GCState {
    Pause,
    Propagate,
    PropagateAgain,
    Atomic,
    Sweep,
}

impl GCState {
    #[inline(always)]
    fn keep_invariant(self) -> bool {
        matches!(self, Self::Propagate | Self::PropagateAgain | Self::Atomic)
    }
}

pub struct Heap<A: Allocator = BumpAllocator> {
    allocator: A,
    objects: Option<GCPtr>,
    gray: Option<GCPtr>,
    gray_again: Option<GCPtr>,
    bytes_allocated: usize,
    gc_threshold: usize,
    gc_state: GCState,
    current_white: GCColor,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            allocator: BumpAllocator::new(),
            objects: None,
            gray: None,
            gray_again: None,
            bytes_allocated: 0,
            gc_threshold: MAX_THRESHOLD,
            gc_state: GCState::Pause,
            current_white: GCColor::White0,
        }
    }

    #[inline]
    pub(super) fn alloc_array(&mut self, is_ptr: bool, elems: &[Value]) -> GCPtr {
        let len = elems.len();
        let size = size_of::<ObjArray>() + (len * size_of::<Value>());
        let layout = Layout::from_size_align(size, align_of::<ObjArray>()).unwrap();

        unsafe {
            let ptr = self.allocator.alloc(layout);

            ptr.cast().write(ObjArray::new(
                GCHeader {
                    color: self.current_white,
                    obj_type: tnr! {is_ptr => ObjType::PtrArray : ObjType::Array},
                    layout,
                    next: self.objects,
                },
                len as u64,
            ));

            let data = ptr.add(size_of::<ObjArray>()).cast().as_ptr();
            std::ptr::copy_nonoverlapping(elems.as_ptr(), data, len);

            let ptr = GCPtr::new(ptr);
            self.objects = Some(ptr);
            self.bytes_allocated += size;

            ptr
        }
    }

    #[inline]
    pub(super) fn alloc_list(&mut self, is_ptr: bool, elems: &[Value]) -> GCPtr {
        let layout = Layout::new::<ObjList>();

        unsafe {
            let ptr = self.allocator.alloc(layout);

            ptr.cast().write(ObjList::new(
                GCHeader {
                    color: self.current_white,
                    obj_type: tnr! {is_ptr => ObjType::PtrList : ObjType::List},
                    layout,
                    next: self.objects,
                },
                elems,
            ));

            let ptr = GCPtr::new(ptr);
            self.objects = Some(ptr);
            self.bytes_allocated += layout.size();

            ptr
        }
    }

    #[inline]
    pub(super) fn alloc_struct(&mut self, ptr_mask: u64, words: &[Value]) -> GCPtr {
        let word_size = words.len();
        let size = size_of::<ObjStruct>() + (word_size * size_of::<Value>());
        let layout = Layout::from_size_align(size, align_of::<ObjStruct>()).unwrap();

        unsafe {
            let ptr = self.allocator.alloc(layout);

            ptr.cast().write(ObjStruct::new(
                GCHeader {
                    color: self.current_white,
                    obj_type: tnr! {ptr_mask != 0 => ObjType::PtrStruct : ObjType::Struct},
                    layout,
                    next: self.objects,
                },
                word_size as u64,
                ptr_mask,
            ));

            let data = ptr.add(size_of::<ObjStruct>()).cast().as_ptr();
            std::ptr::copy_nonoverlapping(words.as_ptr(), data, word_size);

            let ptr = GCPtr::new(ptr);

            self.objects = Some(ptr);
            self.bytes_allocated += size;

            ptr
        }
    }

    #[inline]
    pub(super) fn alloc_string(&mut self, str: &str) -> GCPtr {
        let layout = Layout::new::<ObjString>();

        unsafe {
            let ptr = self.allocator.alloc(layout);

            ptr.cast().write(ObjString::new(
                GCHeader {
                    color: self.current_white,
                    obj_type: ObjType::String,
                    layout,
                    next: self.objects,
                },
                String::from(str),
            ));

            let ptr = GCPtr::new(ptr);
            self.objects = Some(ptr);
            self.bytes_allocated += layout.size();

            ptr
        }
    }

    #[inline]
    pub(super) fn alloc_task(&mut self, data: Task) -> GCPtr {
        let layout = Layout::new::<ObjTask>();

        unsafe {
            let ptr = self.allocator.alloc(layout);

            ptr.cast().write(ObjTask::new(
                GCHeader {
                    color: self.current_white,
                    obj_type: ObjType::Task,
                    layout,
                    next: self.objects,
                },
                data,
            ));

            let ptr = GCPtr::new(ptr);
            self.objects = Some(ptr);
            self.bytes_allocated += layout.size();

            ptr
        }
    }

    fn mark_roots(&mut self, roots: &[Value]) {
        self.gray = None;
        self.gray_again = None;

        for &root in roots {
            self.mark_object(root.get());
        }

        self.gc_state = GCState::Propagate;
    }

    fn mark_object(&mut self, mut obj: GCPtr) {
        let header = obj.hdr_mut();

        if !header.color.is_white() {
            return;
        }

        header.color = GCColor::Gray;

        match header.obj_type {
            ObjType::PtrArray => {
                obj.as_mut::<ObjArray>().gc_list = self.gray;
                self.gray = Some(obj)
            }
            ObjType::PtrList => {
                obj.as_mut::<ObjList>().gc_list = self.gray;
                self.gray = Some(obj)
            }
            ObjType::PtrStruct => {
                obj.as_mut::<ObjStruct>().gc_list = self.gray;
                self.gray = Some(obj)
            }
            ObjType::Task => todo!("mark task"),
            ObjType::String | ObjType::Array | ObjType::List | ObjType::Struct => {
                header.color = GCColor::Black
            }
        }
    }

    fn propagate_mark(&mut self, mut obj: GCPtr) -> usize {
        match obj.ty() {
            ObjType::PtrArray => {
                let arr = obj.as_mut::<ObjArray>();
                self.gray = arr.gc_list;
            }
            ObjType::PtrList => {
                let list = obj.as_mut::<ObjList>();
                self.gray = list.gc_list;
            }
            ObjType::PtrStruct => {
                let st = obj.as_mut::<ObjStruct>();
                self.gray = st.gc_list;
            }
            ObjType::Array | ObjType::List | ObjType::Struct | ObjType::String | ObjType::Task => {
                unreachable!("non-gray object in gray list")
            }
        }

        obj.hdr_mut().color = GCColor::Black;
        self.trace_children(obj);
        obj.hdr().layout.size()
    }

    fn propagate_all(&mut self) -> usize {
        let mut work = 0;

        while let Some(obj) = self.gray {
            work += self.propagate_mark(obj);
        }

        work
    }

    fn trace_children(&mut self, obj: GCPtr) {
        match obj.ty() {
            ObjType::PtrArray => {
                let arr = obj.as_ref::<ObjArray>();
                for v in arr.iter() {
                    self.mark_object(v.get());
                }
            }
            ObjType::PtrList => {
                let list = obj.as_ref::<ObjList>();
                for v in list.iter() {
                    self.mark_object(v.get());
                }
            }
            ObjType::PtrStruct => {
                let st = obj.as_ref::<ObjStruct>();
                for (i, &v) in st.fields().iter().enumerate() {
                    if st.has_ptr_at(i) {
                        self.mark_object(v.get());
                    }
                }
            }
            ObjType::Task => todo!("trace task children"),
            ObjType::String | ObjType::Array | ObjType::List | ObjType::Struct => {}
        }
    }

    fn atomic(&mut self, roots: &[Value]) -> usize {
        let mut work = 0;

        for &root in roots {
            self.mark_object(root.get());
        }

        // TODO: remark upvalues
        // traverse objects caught by write barrier.
        work += self.propagate_all();

        // remark gray again
        std::mem::swap(&mut self.gray, &mut self.gray_again);
        work += self.propagate_all();

        self.current_white = self.current_white.other_white();
        self.gc_state = GCState::Sweep;

        work
    }

    fn sweep(&mut self, limit: usize) -> usize {
        const SWEEP_COST: usize = 16; // Cost per object swept

        let mut work = 0;
        let mut prev = None;
        let mut curr = self.objects;

        while let Some(mut obj) = curr
            && work < limit
        {
            let next = obj.hdr().next;
            let color = obj.hdr().color;

            if color == self.current_white.other_white() {
                self.free_object(obj, prev);
            } else {
                obj.hdr_mut().color = self.current_white;
                prev = Some(obj);
            }

            curr = next;
            work += SWEEP_COST;
        }

        if curr.is_none() {
            self.gc_state = GCState::Pause;
        }

        work
    }

    fn free_object(&mut self, obj: GCPtr, prev: Option<GCPtr>) -> usize {
        match prev {
            Some(mut prev) => prev.hdr_mut().next = obj.hdr().next,
            None => self.objects = obj.hdr().next,
        }

        let layout = obj.hdr().layout;
        let size = layout.size();

        unsafe {
            // Safety: The object is cast to the correct type before dropping
            match obj.ty() {
                ObjType::Array | ObjType::PtrArray => obj.as_ptr::<ObjArray>().drop_in_place(),
                ObjType::List | ObjType::PtrList => obj.as_ptr::<ObjList>().drop_in_place(),
                ObjType::Struct | ObjType::PtrStruct => obj.as_ptr::<ObjStruct>().drop_in_place(),
                ObjType::String => obj.as_ptr::<ObjString>().drop_in_place(),
                ObjType::Task => obj.as_ptr::<ObjTask>().drop_in_place(),
            }

            // Safety:
            // This pointer came from this allocator.
            // The layout of this memory is cached in the header.
            self.allocator.free(obj.as_ptr::<u8>(), layout);
        }

        self.bytes_allocated = self.bytes_allocated.saturating_sub(size);
        size
    }

    fn step_gc(&mut self, roots: &[Value], limit: usize) -> usize {
        let mut cost = 0;

        match self.gc_state {
            GCState::Pause => self.mark_roots(roots),
            GCState::Propagate => {
                while let Some(obj) = self.gray
                    && cost < limit
                {
                    cost += self.propagate_mark(obj)
                }

                if self.gray.is_none() {
                    std::mem::swap(&mut self.gray, &mut self.gray_again);
                    self.gc_state = GCState::PropagateAgain;
                }
            }
            GCState::PropagateAgain => {
                while let Some(obj) = self.gray
                    && cost < limit
                {
                    cost += self.propagate_mark(obj)
                }

                if self.gray.is_none() {
                    self.gc_state = GCState::Atomic;
                }
            }
            GCState::Atomic => cost = self.atomic(roots),
            GCState::Sweep => cost = self.sweep(limit),
        }

        cost
    }

    /// Forward barrier - marks the child object to maintain invariant
    /// Used when: black object gets a new white child reference
    pub fn barrier_forward(&mut self, parent: GCPtr, child: GCPtr) {
        if self.gc_state.keep_invariant()
            && parent.hdr().color.is_black()
            && child.hdr().color.is_white()
        {
            self.mark_object(child);
        }
    }

    /// Backward barrier - marks parent gray again
    /// Used for: table/array/struct writes during Propagate phase
    pub fn barrier_back(&mut self, mut parent: GCPtr) {
        if self.gc_state == GCState::Pause || !parent.hdr().color.is_black() {
            return;
        }

        parent.hdr_mut().color = GCColor::Gray;

        match parent.ty() {
            ObjType::PtrArray => {
                parent.as_mut::<ObjArray>().gc_list = self.gray_again;
                self.gray_again = Some(parent);
            }
            ObjType::PtrList => {
                parent.as_mut::<ObjList>().gc_list = self.gray_again;
                self.gray_again = Some(parent);
            }
            ObjType::PtrStruct => {
                parent.as_mut::<ObjStruct>().gc_list = self.gray_again;
                self.gray_again = Some(parent);
            }
            ObjType::String | ObjType::Array | ObjType::List | ObjType::Struct => {}
            ObjType::Task => todo!("task barrier_back"),
        }
    }

    /// Table barrier - special handling for PropagateAgain phase
    /// During PropagateAgain, use forward barrier; otherwise backward
    pub fn barrier_table(&mut self, parent: GCPtr, child: GCPtr) {
        if self.gc_state == GCState::Pause {
            return;
        }

        if self.gc_state == GCState::PropagateAgain {
            self.barrier_forward(parent, child);
        } else {
            self.barrier_back(parent);
        }
    }

    /// Public GC step function - performs incremental GC work
    /// Returns the amount of work done
    pub fn gc_step(&mut self, roots: &[Value], limit: usize) -> usize {
        // Check if we should start a new cycle
        if self.gc_state == GCState::Pause && self.bytes_allocated > self.gc_threshold {
            self.step_gc(roots, limit);
        }

        // Perform one GC step
        let work = self.step_gc(roots, limit);

        // Update threshold when cycle completes
        if self.gc_state == GCState::Pause {
            self.gc_threshold = (self.bytes_allocated * 2).max(MAX_THRESHOLD);
        }

        work
    }
}
