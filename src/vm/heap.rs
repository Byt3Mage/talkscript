use ahash::AHashSet;
use simple_ternary::tnr;

use crate::{type_registry::TypeRegistry, vm::vm_types::Ptr};

/// ### Layout
/// * type_id(16)
/// * fwd_ptr(46)
/// * fwd(1)
/// * mark(1)
#[repr(transparent)]
#[derive(Clone, Copy)]
struct Header(u64);

impl Header {
    const MARK_BIT: u64 = 1 << 0;
    const FWD_BIT: u64 = 1 << 1;

    // high 16 bits
    const TYPE_SHIFT: u64 = 48;
    const TYPE_MASK: u64 = 0xFFFF;

    // next 46 bits(forwarding offset from base)
    const FWD_SHIFT: u64 = 2;
    const FWD_MASK: u64 = 0x3FFF_FFFF_FFFF; // 46 bits

    #[inline(always)]
    const fn new(type_id: u16) -> Self {
        Self((type_id as u64) << Self::TYPE_SHIFT)
    }

    #[inline(always)]
    const fn type_id(&self) -> u16 {
        ((self.0 >> Self::TYPE_SHIFT) & Self::TYPE_MASK) as u16
    }

    #[inline(always)]
    const fn is_marked(&self) -> bool {
        (self.0 & Self::MARK_BIT) != 0
    }

    #[inline(always)]
    const fn mark(&mut self) {
        self.0 |= Self::MARK_BIT;
    }

    #[inline(always)]
    const fn unmark(&mut self) {
        self.0 &= !Self::MARK_BIT;
    }

    #[inline(always)]
    const fn is_forwarded(&self) -> bool {
        (self.0 & Self::FWD_BIT) != 0
    }

    #[inline(always)]
    const fn clear_forwarded(&mut self) {
        self.0 &= !Self::FWD_BIT;
    }

    #[inline(always)]
    const fn set_forwarding_ptr(&mut self, ptr: Ptr) {
        let pos = ptr.position() & Self::FWD_MASK;
        let old = self.0 & !((Self::FWD_MASK << Self::FWD_SHIFT) | Self::FWD_BIT);
        self.0 = old | (pos << Self::FWD_SHIFT) | Self::FWD_BIT;
    }

    #[inline(always)]
    const fn get_forwarding_ptr(&self) -> Ptr {
        // forwarded pointers are always old
        Ptr::old((self.0 >> Self::FWD_SHIFT) & Self::FWD_MASK)
    }
}

#[inline(always)]
const fn field_slot(ptr: Ptr, offset: u16) -> usize {
    (ptr.position() as usize) + 1 + (offset as usize)
}

pub struct SingleGenHeap {
    young: Vec<u64>,
    young_top: usize,
    old: Vec<u64>,
    old_top: usize,
    remembered: AHashSet<Ptr>,
}

impl SingleGenHeap {
    #[inline(always)]
    pub fn new(young_slots: usize, old_slots: usize) -> Self {
        Self {
            young: vec![0; young_slots],
            young_top: 0,
            old: vec![0; old_slots],
            old_top: 0,
            remembered: AHashSet::new(),
        }
    }

    #[inline]
    pub fn alloc(
        &mut self,
        type_id: u16,
        types: &TypeRegistry,
        roots: &mut [Ptr],
    ) -> Result<Ptr, HeapError> {
        let slots = 1 + types.get_info(type_id).num_fields as usize;

        // Try allocate
        if self.young_top + slots <= self.young.len() {
            let pos = self.young_top;
            self.young_top += slots;
            self.young[pos] = Header::new(type_id).0;
            return Ok(Ptr::nursery(pos as u64));
        }

        // Nursery full, try minor GC
        self.minor_gc(roots, types)?;

        // Retry allocation
        if self.young_top + slots <= self.young.len() {
            let pos = self.young_top;
            self.young_top += slots;
            self.young[pos] = Header::new(type_id).0;
            return Ok(Ptr::nursery(pos as u64));
        }

        Err(HeapError::YoungOutOfMemory)
    }

    #[inline(always)]
    pub fn get_field(&self, ptr: Ptr, offset: u16) -> u64 {
        let slot = field_slot(ptr, offset);
        tnr! {ptr.is_old() => self.old[slot] : self.young[slot]}
    }

    #[inline(always)]
    pub fn get_ptr_field(&self, ptr: Ptr, offset: u16) -> Ptr {
        let slot = field_slot(ptr, offset);
        Ptr::from_raw(tnr! {ptr.is_old() => self.old[slot] : self.young[slot]})
    }

    pub fn set_field(&mut self, ptr: Ptr, offset: u16, value: u64) {
        let slot = field_slot(ptr, offset);
        if ptr.is_old() {
            self.old[slot] = value;
        } else {
            self.young[slot] = value;
        }
    }

    pub fn set_ptr_field(&mut self, ptr: Ptr, offset: u16, value: Ptr) {
        let slot = field_slot(ptr, offset);
        if ptr.is_old() {
            self.old[slot] = value.raw();
            // Write barrier: old -> young pointer
            if !value.is_old() {
                self.remembered.insert(ptr);
            }
        } else {
            self.young[slot] = value.raw();
        }
    }

    fn copy_to_old(&mut self, ptr: Ptr, registry: &TypeRegistry) -> Result<Ptr, HeapError> {
        debug_assert!(!ptr.is_old(), "only young objects should be copied");

        let pos = ptr.position() as usize;
        let hdr = Header(self.young[pos]);

        if hdr.is_forwarded() {
            return Ok(hdr.get_forwarding_ptr());
        }

        let slots = 1 + registry.get_info(hdr.type_id()).num_fields as usize;
        let start = self.old_top;
        let end = start + slots;

        if end > self.old.len() {
            return Err(HeapError::OldOutOfMemory);
        }

        self.old_top = end;
        self.old[start..end].copy_from_slice(&self.young[pos..pos + slots]);

        let new_ptr = Ptr::old(start as u64);

        self.young[pos] = {
            let mut h = hdr;
            h.set_forwarding_ptr(new_ptr);
            h.0
        };

        Ok(new_ptr)
    }

    pub fn minor_gc(&mut self, roots: &mut [Ptr], types: &TypeRegistry) -> Result<(), HeapError> {
        let scan_start = self.old_top;

        for root in roots.iter_mut() {
            if !root.is_old() && (root.position() as usize) < self.young_top {
                *root = self.copy_to_old(*root, types)?;
            }
        }

        for old_ptr in self.remembered.drain().collect::<Vec<_>>() {
            self.scan_object(old_ptr, types)?;
        }

        // Cheney scan: process copied objects
        let mut scan = scan_start;
        while scan < self.old_top {
            let ptr = Ptr::old(scan as u64);
            self.scan_object(ptr, types)?;

            let hdr = Header(self.old[scan]);
            scan += 1 + types.get_info(hdr.type_id()).num_fields as usize;
        }

        self.young_top = 0;

        Ok(())
    }

    fn scan_object(&mut self, ptr: Ptr, types: &TypeRegistry) -> Result<(), HeapError> {
        debug_assert!(ptr.is_old(), "only old objects should be scanned");

        let pos = ptr.position() as usize;
        let info = types.get_info(Header(self.old[pos]).type_id());

        for &idx in &info.ptr_fields {
            let off = pos + 1 + (idx as usize);
            let child = Ptr::from_raw(self.old[off]);

            if !child.is_old() && (child.position() as usize) < self.young_top {
                let new_ptr = self.copy_to_old(child, types)?;
                self.old[off] = new_ptr.raw();
            }
        }

        Ok(())
    }

    pub fn major_gc(&mut self, roots: &mut [Ptr], registry: &TypeRegistry) {
        self.mark(roots, registry);
        self.compute_forwarding(registry);
        self.update_references(roots, registry);
        self.compact(registry);
    }

    fn mark(&mut self, roots: &[Ptr], registry: &TypeRegistry) {
        let mut worklist = Vec::new();

        for &root in roots {
            if root.is_old() {
                worklist.push(root);
            }
        }

        let mut scan = 0;
        while scan < self.young_top {
            let info = registry.get_info(Header(self.young[scan]).type_id());

            for &idx in &info.ptr_fields {
                // Direct indexing. We know we're in nursery.
                let off = scan + 1 + (idx as usize);
                let child = Ptr::from_raw(self.young[off]);

                if child.is_old() {
                    worklist.push(child);
                }
            }

            scan += 1 + (info.num_fields as usize);
        }

        // Cheney's algorithm: Breadth-first tree walk to mark objects.
        while let Some(ptr) = worklist.pop() {
            let pos = ptr.position() as usize;
            let mut hdr = Header(self.old[pos]);

            if hdr.is_marked() {
                continue;
            }

            hdr.mark();
            self.old[pos] = hdr.0;

            let info = registry.get_info(hdr.type_id());

            for &idx in &info.ptr_fields {
                let off = pos + 1 + (idx as usize);
                let child = Ptr::from_raw(self.old[off]);

                if child.is_old() {
                    worklist.push(child)
                }
            }
        }
    }

    fn compute_forwarding(&mut self, registry: &TypeRegistry) {
        let mut pos = 0;
        let mut new_pos = 0;

        while pos < self.old_top {
            let mut hdr = Header(self.old[pos]);
            let slots = 1 + registry.get_info(hdr.type_id()).num_fields as usize;

            if hdr.is_marked() {
                hdr.set_forwarding_ptr(Ptr::old(new_pos as u64));
                self.old[pos] = hdr.0;
                new_pos += slots;
            }

            pos += slots;
        }
    }

    fn update_references(&mut self, roots: &mut [Ptr], registry: &TypeRegistry) {
        for root in roots.iter_mut() {
            if root.is_old() {
                let hdr = Header(self.old[root.position() as usize]);
                if hdr.is_forwarded() {
                    *root = hdr.get_forwarding_ptr()
                }
            }
        }

        let mut scan = 0;
        while scan < self.young_top {
            let hdr = Header(self.young[scan]);
            let info = registry.get_info(hdr.type_id());

            for &idx in &info.ptr_fields {
                let off = scan + 1 + (idx as usize);
                let val = Ptr::from_raw(self.young[off]);

                if val.is_old() {
                    let old_hdr = Header(self.old[val.position() as usize]);
                    if old_hdr.is_forwarded() {
                        self.young[off] = old_hdr.get_forwarding_ptr().raw();
                    }
                }
            }

            scan += 1 + info.num_fields as usize;
        }

        scan = 0;

        while scan < self.old_top {
            let hdr = Header(self.old[scan]);
            let info = registry.get_info(hdr.type_id());

            if hdr.is_marked() {
                for &idx in &info.ptr_fields {
                    let off = scan + 1 + (idx as usize);
                    let val = Ptr::from_raw(self.old[off]);

                    if val.is_old() {
                        let tgt_hdr = Header(self.old[val.position() as usize]);
                        if tgt_hdr.is_forwarded() {
                            self.old[off] = tgt_hdr.get_forwarding_ptr().raw();
                        }
                    }
                }
            }

            scan += 1 + info.num_fields as usize
        }
    }

    fn compact(&mut self, registry: &TypeRegistry) {
        let mut new_pos = 0;
        let mut scan = 0;

        while scan < self.old_top {
            let hdr = Header(self.old[scan]);
            let slots = 1 + registry.get_info(hdr.type_id()).num_fields as usize;

            if hdr.is_marked() {
                // Move object to new location if needed
                if scan != new_pos {
                    self.old.copy_within(scan..scan + slots, new_pos);
                }

                // Clear mark and forwarding bits for next GC cycle
                let mut new_hdr = Header(self.old[new_pos]);
                new_hdr.unmark();
                new_hdr.clear_forwarded();
                self.old[new_pos] = new_hdr.0;

                new_pos += slots;
                scan += slots;
            } else {
                scan += slots;
            }
        }

        self.old_top = new_pos;
    }
}

pub enum HeapError {
    YoungOutOfMemory,
    OldOutOfMemory,
}
