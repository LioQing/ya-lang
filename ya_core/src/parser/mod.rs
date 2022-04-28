use crate::lexer;
use thiserror::Error;
use std::str::FromStr;

#[cfg(test)]
mod tests;

pub mod item;
pub mod stmt;
pub mod expr;
pub mod construct;
pub mod token;

pub use expr::*;
pub use stmt::*;
pub use construct::*;
pub use item::*;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("{0}")]
    Lexer(#[from] lexer::LexerError),

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

pub struct Parser<'a> {
    pub items: Vec<Item>,
    pub errs: Vec<ParserError>,
    pub src: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Parser {
            items: vec![],
            errs: vec![],
            src,
        }
    }

    pub fn parse_items(&mut self) {
        let mut lexer = lexer::Lexer::new(self.src);

        loop {
            match Item::parse(&mut lexer) {
                Ok(Item::Eof) => break,
                Err(e) => self.errs.push(e),
                Ok(i) => self.items.push(i),
            }
        }
    }
}