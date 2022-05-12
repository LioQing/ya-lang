use ya_lexer;
use ya_prim_types::PrimType;
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

#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum Error {
    #[error("{0}")]
    Lexer(#[from] ya_lexer::Error),

    #[error("Enum parse error: {0}")]
    StrumEnumParse(#[from] strum::ParseError),

    #[error("Expected expression, found {found}")]
    ExpectedExpr { found: String },

    #[error("Expected primary expression, found {found}")]
    ExpectedPrimExpr { found: String },

    #[error("Expected identifier, found {found:?}")]
    ExpectedIdentifier { found: ya_lexer::Token },

    #[error("Expected keyword: {expected:?}, found {found:?}")]
    ExpectedKeyword { expected: Vec<String>, found: ya_lexer::Token },

    #[error("Expected symbol: {expected:?}, found {found:?}")]
    ExpectedSymbol { expected: Vec<String>, found: ya_lexer::Token },

    #[error("Expected bracket: {expected:?}, found {found:?}")]
    ExpectedBracket { expected: Vec<char>, found: ya_lexer::Token },

    #[error("Expected separator: {expected}, found {found:?}")]
    ExpectedSeparator { expected: char, found: ya_lexer::Token },

    #[error("Expected operator: {expected:?}, found {found:?}")]
    ExpectedOperator { expected: Vec<String>, found: ya_lexer::Token },

    #[error("Expected literal, found {found:?}")]
    ExpectedLiteral { found: ya_lexer::Token },

    #[error("Expected parameter declaration in format of `[identifier]: [type]`, found {found:?}")]
    ExpectedParamDecl { found: (ya_lexer::Token, ya_lexer::Token, ya_lexer::Token) },

    #[error("Ambiguous operators")]
    AmbiguousOperators,

    #[error("Unknown token {token:?} in global scope")]
    UnknownTokenInGlobalScope { token: ya_lexer::Token },
}

/// The syntax parser.
/// 
/// Performs syntactical analysis.
/// Parse the tokens from lexer into a parse tree.
pub struct Parser {
    pub items: Vec<Item>,
}

impl Parser {
    pub fn parse(src: &str) -> Self {
        let mut lexer = ya_lexer::Lexer::new(src);
        let mut items = vec![];

        loop {
            match Item::parse(&mut lexer) {
                Item::Eof => break,
                i => items.push(i),
            }
        }

        Self { items }
    }
}