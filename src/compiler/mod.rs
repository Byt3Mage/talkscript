mod ast;
mod lexer;
mod parse_rules;
mod parser;
mod resolver;
mod tokens;
mod type_checker;

#[cfg(test)]
use crate::{
    arena::Interner,
    compiler::{
        ast::AstArena, lexer::Lexer, parser::Parser, resolver::Resolver, type_checker::TypeChecker,
    },
};

#[test]
fn test_all() {
    let source = r#"
        struct Point {
            x: float,
            y: float,
            debug: str,
        }

        fn add(var a: int, b: int, c: float) -> int {
            a += 5;
            a + b + c
        }
    "#;

    let mut interner = Interner::new();
    let mut ast = AstArena::new();
    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(&mut lexer, &mut interner, &mut ast).unwrap();

    let module = parser.parse_source().unwrap();

    let mut resolver = Resolver::new(&ast);
    resolver.resolve_item(&module);

    if !resolver.errors.is_empty() {
        for e in resolver.errors {
            let msg = e.display(&interner);
            println!("resolver error: {msg}");
        }
    } else {
        let mut type_checker = TypeChecker::new(&interner, resolver.symbols);
        type_checker.check_item(&module);
        for e in type_checker.errors() {
            let msg = e.display(&interner, &ast);
            println!("type error: {msg}")
        }
    }
}
