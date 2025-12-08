use std::{alloc::Layout, ptr::NonNull};

use crate::vm::heap::GCHeader;

/// Configuration for size-segregated allocation.
/// Maps allocation sizes to size classes to reduce fragmentation.
pub struct SizeClassConfig {
    /// The actual size (in bytes) for each size class
    size_of_class: [usize; SIZE_CLASSES],

    /// Maps a requested size to its size class index
    /// -1 means "too large, use a dedicated allocation"
    class_for_size: [i8; MAX_SMALL_SIZE + 1],

    /// Number of size classes actually used
    class_count: usize,
}

const SIZE_CLASSES: usize = 40;
const MAX_SMALL_SIZE: usize = 1024;

impl SizeClassConfig {
    /// Creates a new size class configuration with progressive size classes:
    /// - 8b increments up to 64b (8, 16, 24, 32, 40, 48, 56)
    /// - 16b increments up to 256b (64, 80, 96, ..., 240)
    /// - 32b increments up to 512b (256, 288, 320, ..., 480)
    /// - 64b increments up to 1024b (512, 576, 640, ..., 1024)
    pub const fn new() -> Self {
        let mut config = Self {
            size_of_class: [0; SIZE_CLASSES],
            class_for_size: [-1; MAX_SMALL_SIZE + 1],
            class_count: 0,
        };

        // 8-byte increments: 8, 16, 24, 32, 40, 48, 56
        let mut size = 8;
        while size < 64 {
            config.size_of_class[config.class_count] = size;
            config.class_count += 1;
            size += 8;
        }

        // 16-byte increments: 64, 80, 96, ..., 240
        size = 64;
        while size < 256 {
            config.size_of_class[config.class_count] = size;
            config.class_count += 1;
            size += 16;
        }

        // 32-byte increments: 256, 288, 320, ..., 480
        size = 256;
        while size < 512 {
            config.size_of_class[config.class_count] = size;
            config.class_count += 1;
            size += 32;
        }

        // 64-byte increments: 512, 576, 640, ..., 1024
        size = 512;
        while size <= 1024 {
            config.size_of_class[config.class_count] = size;
            config.class_count += 1;
            size += 64;
        }

        // Fill the lookup table for exact size class matches
        let mut class = 0;
        while class < config.class_count {
            let size = config.size_of_class[class];
            config.class_for_size[size] = class as i8;
            class += 1;
        }

        // Fill gaps in lookup table (round up to next size class)
        let mut size = MAX_SMALL_SIZE;
        while size > 0 {
            if config.class_for_size[size] < 0 {
                config.class_for_size[size] = config.class_for_size[size + 1];
            }
            size -= 1;
        }

        config
    }
    /// Returns the size class index for a given allocation size.
    /// Returns -1 if the size is too large for paged allocation.
    #[inline]
    pub const fn size_class(&self, size: usize) -> i8 {
        if size == 0 {
            // Empty allocations take no space
            -1
        } else if size <= MAX_SMALL_SIZE {
            self.class_for_size[size]
        } else {
            // Too large for size classes
            -1
        }
    }

    /// Returns the actual block size for a given size class.
    #[inline]
    pub const fn block_size(&self, class: usize) -> usize {
        self.size_of_class[class]
    }

    /// Returns the number of active size classes.
    #[inline]
    pub const fn count(&self) -> usize {
        self.class_count
    }
}

/// Global size class configuration
pub static SIZE_CLASS_CONFIG: SizeClassConfig = SizeClassConfig::new();

type PagePtr = NonNull<Page>;

/// A page of memory containing multiple blocks of the same size.
/// Pages are linked in two intrusive linked lists:
/// 1. Freelist - pages with at least one free block (for fast allocation)
/// 2. All-pages list - all pages (for GC sweeping)
#[repr(C)]
pub struct Page {
    // Freelist linkage (pages with free blocks)
    prev: Option<PagePtr>,
    next: Option<PagePtr>,

    // All-pages list linkage (for sweeping)
    list_prev: Option<PagePtr>,
    list_next: Option<PagePtr>,

    // Page config
    page_size: u32,
    block_size: u32,

    // Allocation state
    free_list: Option<NonNull<u8>>, // Linked list of freed blocks
    free_next: i32,                 // Bump allocator offset (negative when exhausted)
    busy_blocks: u32,               // Number of allocated blocks

    // Padding to align data to 16 bytes
    _padding: [u8; 4],

    // Block data follows (flexible array member)
    data: [u8; 0],
}

// Verify alignment requirement
const _: () = {
    assert!(
        std::mem::size_of::<Page>() % 16 == 0,
        "Page header must be 16-byte aligned"
    );
};

const SMALL_PAGE_SIZE: usize = 16 * 1024;
const LARGE_PAGE_SIZE: usize = 32 * 1024;
const LARGE_PAGE_THRESHOLD: usize = 512;

// Offset where freelist link is stored in freed GCO blocks
// Must be after GCHeader, aligned to pointer size
const GCO_LINK_OFFSET: usize = {
    let header_size = std::mem::size_of::<GCHeader>();
    let ptr_size = std::mem::size_of::<*mut u8>();

    // Round up to pointer alignment
    (header_size + ptr_size - 1) & !(ptr_size - 1)
};

impl Page {
    /// Calculate page size for a given block size
    #[inline]
    pub const fn page_size_for_block(block_size: usize) -> usize {
        if block_size > LARGE_PAGE_THRESHOLD {
            LARGE_PAGE_SIZE
        } else {
            SMALL_PAGE_SIZE
        }
    }

    /// Calculate how many blocks fit in a page
    #[inline]
    pub const fn blocks_per_page(page_size: usize, block_size: usize) -> usize {
        let data_size = page_size - std::mem::size_of::<Page>();
        data_size / block_size
    }

    /// Get pointer to page's data area
    #[inline]
    pub fn data_ptr(&self) -> *mut u8 {
        self.data.as_ptr().cast_mut()
    }

    /// Get pointer to a specific block by index
    #[inline]
    pub unsafe fn block_ptr(&self, index: usize) -> *mut u8 {
        unsafe { self.data_ptr().add(index * self.block_size as usize) }
    }

    /// Check if a pointer belongs to this page
    #[inline]
    pub fn contains(&self, ptr: *const u8) -> bool {
        let page_start = self as *const Page as *const u8;
        let page_end = unsafe { page_start.add(self.page_size as usize) };
        let data_start = self.data_ptr() as *const u8;

        ptr >= data_start && ptr < page_end
    }

    /// Get the block index for a pointer (assumes ptr is in this page)
    #[inline]
    pub unsafe fn block_index(&self, ptr: *const u8) -> usize {
        let offset = unsafe { ptr.offset_from(self.data_ptr()) } as usize;
        offset / self.block_size as usize
    }

    /// Check if page has any free blocks
    #[inline]
    pub fn has_free_blocks(&self) -> bool {
        self.free_list.is_some() || self.free_next >= 0
    }

    /// Check if page is empty (all blocks freed)
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.busy_blocks == 0
    }

    /// Check if page is full (no free blocks)
    #[inline]
    pub fn is_full(&self) -> bool {
        !self.has_free_blocks()
    }
}

pub enum PageAllocError {
    OutOfMemory,
    InvalidSize,
}

impl Page {
    /// Allocate and initialize a new page
    pub fn new(block_size: usize, page_size: usize) -> Result<PagePtr, PageAllocError> {
        if block_size == 0 || page_size == 0 || page_size <= std::mem::size_of::<Page>() {
            return Err(PageAllocError::InvalidSize);
        }

        // Calculate how many blocks fit
        let data_size = page_size - std::mem::size_of::<Page>();
        let block_count = data_size / block_size;

        if block_count == 0 {
            return Err(PageAllocError::InvalidSize);
        }

        let layout =
            Layout::from_size_align(page_size, 16).map_err(|_| PageAllocError::InvalidSize)?;

        // TODO: use allocator
        let ptr = match NonNull::new(unsafe { std::alloc::alloc(layout) }) {
            Some(p) => p.cast(),
            None => return Err(PageAllocError::OutOfMemory),
        };

        unsafe {
            ptr.write(Page {
                prev: None,
                next: None,
                list_prev: None,
                list_next: None,
                page_size: page_size as u32,
                block_size: block_size as u32,
                free_list: None,
                // Start bump allocator at last block, count down
                free_next: ((block_count - 1) * block_size) as i32,
                busy_blocks: 0,
                _padding: [0; 4],
                data: [],
            });
        }

        Ok(ptr)
    }

    /// Deallocate a page
    ///
    /// # Safety
    /// * Page must have been allocated by `Page::new`
    /// * Page must not be in any linked lists
    /// * All blocks in the page must be freed (busy_blocks == 0)
    pub unsafe fn dealloc(page: NonNull<Page>) {
        unsafe {
            let page_ref = page.as_ref();

            debug_assert!(
                page_ref.busy_blocks == 0,
                "Deallocating page with busy blocks"
            );

            debug_assert!(
                page_ref.prev.is_none() && page_ref.next.is_none(),
                "Page still in freelist"
            );

            let page_size = page_ref.page_size as usize;
            let layout = Layout::from_size_align_unchecked(page_size, 16);

            // todo: use allocator interface
            std::alloc::dealloc(page.as_ptr() as *mut u8, layout);
        }
    }

    /// Create a page for a specific size class
    pub fn new_for_size_class(size_class: usize) -> Result<PagePtr, PageAllocError> {
        let block_size = SIZE_CLASS_CONFIG.block_size(size_class);
        let page_size = Self::page_size_for_block(block_size);

        Self::new(block_size, page_size)
    }

    /// Create a dedicated page for a single large object
    ///
    /// Used when object size exceeds MAX_SMALL_SIZE (1024 bytes)
    pub fn new_large(object_size: usize) -> Result<PagePtr, PageAllocError> {
        // Page contains exactly one block
        let page_size = std::mem::size_of::<Page>() + object_size;

        // Round up to 16-byte alignment
        let page_size = (page_size + 15) & !15;

        Self::new(object_size, page_size)
    }

    /// Allocate a block from this page
    ///
    /// # Returns
    /// * `Some(NonNull<u8>)` - Pointer to allocated block
    /// * `None` - Page is full (no free blocks)
    ///
    /// # Safety
    /// Caller must ensure the page is valid and properly initialized
    pub unsafe fn alloc_block(&mut self) -> Option<NonNull<u8>> {
        debug_assert!(self.has_free_blocks(), "alloc_block called on full page");

        unsafe {
            let block_ptr = if self.free_next >= 0 {
                // Use bump allocator (fast path)
                let ptr = self.data_ptr().add(self.free_next as usize);
                self.free_next -= self.block_size as i32;
                ptr
            } else if let Some(free_block) = self.free_list {
                // Use freelist (reuse freed block)
                let ptr = free_block.as_ptr();

                // Read next pointer from GCO_LINK_OFFSET
                let next_ptr = *(ptr.add(GCO_LINK_OFFSET) as *const *mut u8);
                self.free_list = NonNull::new(next_ptr);

                ptr
            } else {
                // No free blocks available
                return None;
            };

            self.busy_blocks += 1;

            // Zero the block for safety
            std::ptr::write_bytes(block_ptr, 0, self.block_size as usize);

            Some(NonNull::new_unchecked(block_ptr))
        }
    }

    /// Free a block in this page
    ///
    /// # Safety
    /// * `block` must point to a valid block within this page
    /// * Block must currently be allocated (not already freed)
    /// * For GCO blocks, caller must set type to NIL before calling
    pub unsafe fn free_block(&mut self, block: NonNull<u8>) {
        debug_assert!(self.contains(block.as_ptr()), "Block not in this page");
        debug_assert!(self.busy_blocks > 0, "Freeing block from empty page");

        unsafe {
            let ptr = block.as_ptr();

            // Add to freelist - store link at GCO_LINK_OFFSET
            let link_ptr = ptr.add(GCO_LINK_OFFSET) as *mut *mut u8;
            *link_ptr = self.free_list.map_or(std::ptr::null_mut(), |p| p.as_ptr());

            self.free_list = Some(block);
            self.busy_blocks -= 1;
        }
    }

    /// Check if this block belongs to this page and is properly aligned
    ///
    /// # Safety
    /// Caller must ensure page pointer is valid
    pub unsafe fn validate_block(&self, block: *const u8) -> bool {
        if !self.contains(block) {
            return false;
        }

        // Check alignment - block must be at a block boundary
        unsafe {
            let offset = block.offset_from(self.data_ptr()) as usize;
            offset % self.block_size as usize == 0
        }
    }
}

/// Manages lists of pages for allocation
struct PageLists {
    /// Pages with free blocks, indexed by size class
    free_pages: [Option<PagePtr>; SIZE_CLASSES],

    /// All GCO pages (for sweeping)
    all_gco_pages: Option<PagePtr>,
}

impl PageLists {
    pub const fn new() -> Self {
        Self {
            free_pages: [None; SIZE_CLASSES],
            all_gco_pages: None,
        }
    }

    /// Add a page to the freelist for its size class
    ///
    /// # Safety
    /// * Page must not already be in any freelist
    /// * Page must have free blocks available
    /// * Page must remain valid for the lifetime of the PageLists
    pub unsafe fn add_to_freelist(&mut self, mut page: PagePtr, size_class: usize) {
        debug_assert!(size_class < SIZE_CLASSES, "Invalid size class");

        unsafe {
            let page_ref = page.as_mut();
            debug_assert!(page_ref.has_free_blocks(), "Adding full page to freelist");
            debug_assert!(page_ref.prev.is_none(), "Page already has prev link");
            debug_assert!(page_ref.next.is_none(), "Page already has next link");

            // Insert at head of freelist
            page_ref.next = self.free_pages[size_class];
            page_ref.prev = None;

            if let Some(mut old_head) = self.free_pages[size_class] {
                old_head.as_mut().prev = Some(page);
            }

            self.free_pages[size_class] = Some(page);
        }
    }

    /// Remove a page from the freelist for its size class
    ///
    /// # Safety
    /// * Page must be in the freelist for the given size class
    /// * Page must remain valid
    pub unsafe fn remove_from_freelist(&mut self, mut page: PagePtr, size_class: usize) {
        debug_assert!(size_class < SIZE_CLASSES, "Invalid size class");

        unsafe {
            let page_ref = page.as_mut();

            // Update next's prev pointer
            if let Some(mut next) = page_ref.next {
                next.as_mut().prev = page_ref.prev;
            }

            // Update prev's next pointer, or update list head
            if let Some(mut prev) = page_ref.prev {
                prev.as_mut().next = page_ref.next;
            } else {
                // This was the head of the list
                debug_assert_eq!(
                    self.free_pages[size_class].map(|p| p.as_ptr()),
                    Some(page.as_ptr()),
                    "Page not at head of freelist but has no prev"
                );
                self.free_pages[size_class] = page_ref.next;
            }

            // Clear links
            page_ref.prev = None;
            page_ref.next = None;
        }
    }

    /// Get a page with free blocks for the given size class
    ///
    /// Returns the first page in the freelist, or None if no pages available
    pub fn get_free_page(&self, size_class: usize) -> Option<PagePtr> {
        debug_assert!(size_class < SIZE_CLASSES, "Invalid size class");
        self.free_pages[size_class]
    }

    /// Add a page to the all-pages list
    ///
    /// # Safety
    /// * Page must not already be in the all-pages list
    /// * Page must remain valid for the lifetime of the PageLists
    pub unsafe fn add_to_all_pages(&mut self, mut page: NonNull<Page>) {
        unsafe {
            let page_ref = page.as_mut();

            debug_assert!(
                page_ref.list_prev.is_none() && page_ref.list_next.is_none(),
                "Page already has list_prev"
            );

            // Insert at head of all-pages list
            page_ref.list_next = self.all_gco_pages;
            page_ref.list_prev = None;

            if let Some(mut old_head) = self.all_gco_pages {
                old_head.as_mut().list_prev = Some(page);
            }

            self.all_gco_pages = Some(page);
        }
    }

    /// Remove a page from the all-pages list
    ///
    /// # Safety
    /// * Page must be in the all-pages list
    pub unsafe fn remove_from_all_pages(&mut self, mut page: NonNull<Page>) {
        unsafe {
            let page_ref = page.as_mut();

            // Update next's prev pointer
            if let Some(mut next) = page_ref.list_next {
                next.as_mut().list_prev = page_ref.list_prev;
            }

            // Update prev's next pointer, or update list head
            if let Some(mut prev) = page_ref.list_prev {
                prev.as_mut().list_next = page_ref.list_next;
            } else {
                // This was the head of the list
                debug_assert_eq!(
                    self.all_gco_pages.map(|p| p.as_ptr()),
                    Some(page.as_ptr()),
                    "Page not at head of all-pages list but has no list_prev"
                );
                self.all_gco_pages = page_ref.list_next;
            }

            // Clear links
            page_ref.list_prev = None;
            page_ref.list_next = None;
        }
    }
}
