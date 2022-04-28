extern crate ya_core;

use ya_core::*;

fn main() {
    let src = std::fs::read_to_string("ya_core/examples/hello_world.ya").unwrap();

    let mut parser = parser::Parser::new(&src);

    parser.parse_items();

    println!("{:#?}", parser.items);
    println!("{:#?}", parser.errs);
}