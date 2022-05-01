extern crate ya_core;

use ya_core::*;

fn main() {
    let src = std::fs::read_to_string("ya_core/examples/hello_world.ya").unwrap();

    let synt_parser = syntax_parser::Parser::parse(&src);

    println!("{:#?}", synt_parser.items);
    println!("{:#?}", synt_parser.errs);

    let sem_parser = semantic_parser::Parser::parse(&synt_parser.items);

    println!("{:#?}", sem_parser.global);
    println!("{:#?}", sem_parser.errs);
}