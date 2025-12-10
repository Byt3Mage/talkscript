use crate::{
    arena::Interner,
    compiler::{
        ast::{AstArena, ItemId, ItemKind},
        resolver::SymbolTable,
    },
};

struct TypeError {}

struct TypeChecker<'a> {
    ast: &'a AstArena,
    interner: &'a Interner,
    symbols: SymbolTable,
}

impl<'a> TypeChecker<'a> {
    fn infer_item(&self, item_id: ItemId) {
        let item = &self.ast.items[item_id];

        match &item.kind {
            ItemKind::Module { items } => {
                for child_id in items {
                    self.infer_item(*child_id);
                }
            }
            ItemKind::Function { params, ret, body } => {
                
            },
            ItemKind::Struct { fields } => todo!(),
        }
    }
}
