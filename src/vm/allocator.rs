use std::{alloc::Layout, ptr::NonNull};

pub trait Allocator {
    unsafe fn alloc(&mut self, layout: Layout) -> NonNull<u8>;

    unsafe fn free(&mut self, ptr: NonNull<u8>, layout: Layout);

    unsafe fn reset(&mut self) {}
}

pub struct BumpAllocator {
    current_chunk: Option<NonNull<u8>>,
    offset: usize,
    end: usize,
    chunks: Vec<Chunk>,
}

struct Chunk {
    ptr: NonNull<u8>,
    layout: Layout,
}

impl BumpAllocator {
    pub fn new() -> Self {
        Self {
            current_chunk: None,
            offset: 0,
            end: 0,
            chunks: vec![],
        }
    }

    #[cold]
    fn alloc_slow(&mut self, layout: Layout) -> NonNull<u8> {
        const MIN_CHUNK_SIZE: usize = 64 * 1024;
        const MIN_CHUNK_ALIGN: usize = 16;

        let chunk_size = layout.size().max(MIN_CHUNK_SIZE).next_power_of_two();
        let chunk_align = layout.align().max(MIN_CHUNK_ALIGN);
        let chunk_layout = Layout::from_size_align(chunk_size, chunk_align)
            .unwrap()
            .pad_to_align();

        let ptr = match NonNull::new(unsafe { std::alloc::alloc(chunk_layout) }) {
            Some(ptr) => ptr,
            None => std::alloc::handle_alloc_error(chunk_layout),
        };

        self.current_chunk = Some(ptr);
        self.offset = layout.size();
        self.end = chunk_size;
        self.chunks.push(Chunk {
            ptr,
            layout: chunk_layout,
        });

        ptr
    }
}

impl Drop for BumpAllocator {
    fn drop(&mut self) {
        self.chunks
            .iter()
            .for_each(|c| unsafe { std::alloc::dealloc(c.ptr.as_ptr(), c.layout) });
    }
}

#[inline(always)]
const fn align_up(value: usize, align: usize) -> usize {
    (value + align - 1) & !(align - 1)
}

impl Allocator for BumpAllocator {
    #[inline]
    unsafe fn alloc(&mut self, layout: Layout) -> NonNull<u8> {
        if let Some(base) = self.current_chunk {
            let aligned_offset = align_up(self.offset, layout.align());
            let needed = aligned_offset + layout.size();
            if needed <= self.end && needed >= aligned_offset {
                self.offset = needed;
                return unsafe { base.add(aligned_offset) };
            }
        }

        self.alloc_slow(layout)
    }

    // Bump allocator does nothing with free
    // Accepts fragmentation.
    unsafe fn free(&mut self, _ptr: NonNull<u8>, _: Layout) {}
}

pub struct SystemAllocator;

impl Allocator for SystemAllocator {
    #[inline]
    unsafe fn alloc(&mut self, layout: Layout) -> NonNull<u8> {
        match NonNull::new(unsafe { std::alloc::alloc(layout) }) {
            Some(ptr) => ptr,
            None => std::alloc::handle_alloc_error(layout),
        }
    }

    unsafe fn free(&mut self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { std::alloc::dealloc(ptr.as_ptr(), layout) };
    }
}
