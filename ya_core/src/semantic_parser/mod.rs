use crate::syntax_parser;
use thiserror::Error;

pub mod item;

pub use item::*;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("error: {0}")]
    Syntax(#[from] syntax_parser::Error),
}

/// The semantic parser.
/// 
/// Performs semantic analysis.
/// Parse the parse tree into an AST.
pub struct Parser {
    pub items: Vec<Item>,
    pub errs: Vec<Error>,
}