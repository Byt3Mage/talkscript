mod ast;
mod lexer;
mod parse_rules;
pub mod parser;
mod token;
mod type_checker;
pub mod type_registry;
pub mod vm;

#[test]
fn test_sum() {
    let n = 100;
    let mut sum = 0;
    let mut i = 0;

    while (i < n) {
        sum += i;
        i += 1;
    }

    println!("sum is: {sum}");
}
