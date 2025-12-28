mod ast;
mod lexer;
mod name_resolver;
mod parse_rules;
mod parser;
mod tokens;
mod type_checker;
mod type_info;

#[cfg(test)]
use crate::{
    arena::Interner,
    compiler::{ast::AstArena, lexer::Lexer, parser::Parser},
};

#[test]
fn test_all() {
    let source = r#"
        struct Point {
            x: float,
            y: float,
            debug: str,
        }

        fn add(var a: int, var b: int, c: float) -> int {
            a += 5;

            loop {
                if (a % 2) == 0 {
                    b += 6;
                }

                if (b % 7) == 0 {
                    break;
                }
            }

            a + b + c
        }
    "#;

    let mut interner = Interner::new();
    let mut ast = AstArena::new();
    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(&mut lexer, &mut interner, &mut ast).unwrap();
    let module = parser.parse_source().unwrap();
}
