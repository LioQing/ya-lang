use crate::lexer;
use crate::prim_type::PrimType;
use thiserror::Error;
use std::str::FromStr;

#[cfg(test)]
mod tests;

pub mod item;
pub mod expr;
pub mod construct;
pub mod token;

pub use expr::*;
pub use construct::*;
pub use item::*;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("{0}")]
    Lexer(#[from] lexer::Error),

    #[error("Enum parse error: {0}")]
    StrumEnumParse(#[from] strum::ParseError),

    #[error("Expected expression, found {found}")]
    ExpectedExpr { found: String },

    #[error("Expected primary expression, found {found}")]
    ExpectedPrimExpr { found: String },

    #[error("Expected identifier, found {found:?}")]
    ExpectedIdentifier { found: lexer::Token },

    #[error("Expected keyword: {expected:?}, found {found:?}")]
    ExpectedKeyword { expected: Vec<String>, found: lexer::Token },

    #[error("Expected symbol: {expected:?}, found {found:?}")]
    ExpectedSymbol { expected: Vec<String>, found: lexer::Token },

    #[error("Expected bracket: {expected:?}, found {found:?}")]
    ExpectedBracket { expected: Vec<char>, found: lexer::Token },

    #[error("Expected separator: {expected}, found {found:?}")]
    ExpectedSeparator { expected: char, found: lexer::Token },

    #[error("Expected operator: {expected:?}, found {found:?}")]
    ExpectedOperator { expected: Vec<String>, found: lexer::Token },

    #[error("Expected literal, found {found:?}")]
    ExpectedLiteral { found: lexer::Token },

    #[error("Expected parameter declaration in format of `[identifier]: [type]`, found {found:?}")]
    ExpectedParamDecl { found: (lexer::Token, lexer::Token, lexer::Token) },

    #[error("Unknown token {token:?} in global scope")]
    UnknownTokenInGlobalScope { token: lexer::Token },
}

/// The syntax parser.
/// 
/// Performs syntactical analysis.
/// Parse the tokens from lexer into a parse tree.
pub struct Parser {
    pub items: Vec<Item>,
    pub errs: Vec<Error>,
}

impl Parser {
    pub fn parse(src: &str) -> Self {
        let mut lexer = lexer::Lexer::new(src);
        let mut items = vec![];
        let mut errs = vec![];

        loop {
            match Item::parse(&mut lexer) {
                Ok(Item::Eof) => break,
                Err(e) => errs.push(e),
                Ok(i) => items.push(i),
            }
        }

        Self { items, errs }
    }
}