extern crate ya_core;

use ya_core::*;

fn main() {
    let src = std::fs::read_to_string("ya_core/examples/hello_world.ya").unwrap();

    let parser = syntax_parser::Parser::parse(&src);

    println!("{:#?}", parser.items);
    println!("{:#?}", parser.errs);
}