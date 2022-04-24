extern crate ya_core;

use ya_core::*;

fn main() {
    let mut lexer = lexer::Lexer::new("() ({([] )} {}) 0o1234567890abcdefg");

    loop {
        let tok = lexer.next_token();
        println!("{:?}", tok);

        if let Ok(lexer::Token::Eof) = tok {
            break;
        }
    }
}
