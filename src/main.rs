extern crate ya_core;

use ya_core::*;

fn main() {
    let src = std::fs::read_to_string("ya_core/examples/hello_world.ya").unwrap();

    let mut parser = parser::Parser::new(&src);

    parser.parse_item_decl();
    parser.parse_item_def();

    println!("{:#?}", parser.items);
    println!("{:#?}", parser.errs);
}

// fn main() {
//     let src = std::fs::read_to_string("ya_core/examples/hello_world.ya").unwrap();
//     let mut lexer = lexer::Lexer::new(&src);
    
//     loop {
//         lexer.peek_token();
//         match lexer.next_token() {
//             Ok(lexer::Token::Eof) => break,
//             tok => println!("{:?}", tok),
//         }
//     }
// }
