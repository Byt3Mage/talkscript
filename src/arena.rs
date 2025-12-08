use slotmap::SlotMap;
use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolU32};

pub type StrSymbol = SymbolU32;
pub type Interner = StringInterner<StringBackend<StrSymbol>>;

pub type Arena<K, T> = SlotMap<K, T>;
