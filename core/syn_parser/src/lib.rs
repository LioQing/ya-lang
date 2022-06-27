use token::*;

#[cfg(test)]
mod tests;

pub mod expr;
pub use expr::*;

pub mod error;
pub use error::*;

// pub fn parse(code: &str) -> Vec<Expr> {
//     let mut lexer = lexer::Lexer::new(code).peekable();

//     unimplemented!()
// }
