use std::{iter::FusedIterator, rc::Rc};

/// Tracks which of the 256 registers contain pointers at a safepoint.
///
/// INVARIANT: A bit is set if that register holds a GC-managed pointer
/// at the corresponding safepoint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PointerMap {
    bits: [u64; 4],
}

impl PointerMap {
    #[inline]
    pub const fn empty() -> Self {
        Self { bits: [0; 4] }
    }

    /// Mark a register as containing a pointer
    #[inline]
    pub fn set_ptr(&mut self, reg: u8) {
        let word = (reg / 64) as usize;
        let bit = reg % 64;
        self.bits[word] |= 1u64 << bit;
    }

    /// Check if a register contains a pointer
    #[inline]
    pub fn is_ptr(&self, reg: u8) -> bool {
        let word = (reg / 64) as usize;
        let bit = reg % 64;
        (self.bits[word] & (1 << bit)) != 0
    }

    /// Clear all bits (for reuse)
    #[inline]
    pub fn clear(&mut self) {
        self.bits = [0; 4];
    }

    /// Count total number of pointer registers
    #[inline]
    pub fn num_ptrs(&self) -> u32 {
        self.bits.iter().map(|word| word.count_ones()).sum()
    }

    pub fn iter(&self) -> PtrMapIter<'_> {
        PtrMapIter {
            map: self,
            word_idx: 0,
            current_word: self.bits[0],
        }
    }
}

pub struct PtrMapIter<'a> {
    map: &'a PointerMap,
    word_idx: usize,
    current_word: u64,
}

impl Iterator for PtrMapIter<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current_word != 0 {
                let bit_pos = self.current_word.trailing_zeros() as u8;
                self.current_word &= self.current_word - 1;
                return Some((self.word_idx as u8 * 64) + bit_pos);
            }

            self.word_idx += 1;
            self.current_word = *self.map.bits.get(self.word_idx)?;
        }
    }
}

impl FusedIterator for PtrMapIter<'_> {}

/// A single safepoint entry mapping a PC to live pointer registers
///
/// INVARIANTS:
/// - `pc` is a valid bytecode offset where GC can safely occur
/// - `ptr_map` accurately reflects which registers hold pointers at this PC
/// - This entry is immutable after creation (for thread-safety during GC)
#[derive(Debug, Clone)]
pub struct SafePoint {
    /// Bytecode program counter where this safepoint occurs
    pub pc: usize,
    /// Bitmask of registers containing pointers at this PC
    pub ptr_map: PointerMap,
}

impl SafePoint {
    #[inline]
    pub const fn new(pc: usize) -> Self {
        Self {
            pc,
            ptr_map: PointerMap::empty(),
        }
    }

    #[inline]
    pub fn add_ptr_reg(&mut self, reg: u8) {
        self.ptr_map.set_ptr(reg);
    }

    pub fn with_ptr_reg(mut self, reg: u8) -> Self {
        self.add_ptr_reg(reg);
        self
    }

    #[inline]
    pub fn is_ptr(&self, reg: u8) -> bool {
        self.ptr_map.is_ptr(reg)
    }
}

pub struct SPTableBuilder {
    entries: Vec<SafePoint>,
}

impl SPTableBuilder {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            entries: Vec::with_capacity(cap),
        }
    }

    /// Add a safepoint entry (called during compilation)
    pub fn add_entry(&mut self, entry: SafePoint) {
        self.entries.push(entry);
    }

    pub fn build(mut self) -> SafePointTable {
        self.entries.sort_unstable_by_key(|e| e.pc);

        SafePointTable {
            entries: self.entries.into(),
        }
    }
}

/// Complete register map table for a function
#[derive(Debug, Clone)]
pub struct SafePointTable {
    /// All safepoint entries, sorted by PC for binary search
    entries: Rc<[SafePoint]>,
}

impl SafePointTable {
    /// Look up the register map for a given `pc`
    ///
    /// Returns [None] if `pc` is not a safepoint
    pub fn lookup(&self, pc: usize) -> Option<&SafePoint> {
        self.entries
            .binary_search_by_key(&pc, |e| e.pc)
            .ok()
            .map(|idx| &self.entries[idx])
    }

    /// Get pointer register map at a `pc`
    #[inline]
    pub fn get_reg_map(&self, pc: usize) -> Option<&PointerMap> {
        self.lookup(pc).map(|entry| &entry.ptr_map)
    }

    /// Check if a `pc` is a safepoint
    #[inline]
    pub fn is_safepoint(&self, pc: usize) -> bool {
        self.lookup(pc).is_some()
    }
}
