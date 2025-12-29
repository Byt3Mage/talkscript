use slotmap::SlotMap;
use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolU32};

pub type StrSymbol = SymbolU32;
pub type Ident = SymbolU32;
pub type Arena<K, T> = SlotMap<K, T>;

pub struct Interner {
    interner: StringInterner<StringBackend<StrSymbol>>,
    anon_name: Ident,
}

impl Interner {
    pub fn new() -> Self {
        let mut interner = StringInterner::new();
        let anon_name = interner.get_or_intern_static("<anon>");

        Self {
            interner,
            anon_name,
        }
    }

    pub fn anon_name(&self) -> Ident {
        self.anon_name
    }

    pub fn resolve(&self, symbol: StrSymbol) -> Option<&str> {
        self.interner.resolve(symbol)
    }

    pub fn get_or_intern(&mut self, str: &str) -> StrSymbol {
        self.interner.get_or_intern(str)
    }

    pub fn get_or_intern_static(&mut self, str: &'static str) -> StrSymbol {
        self.interner.get_or_intern_static(str)
    }
}
