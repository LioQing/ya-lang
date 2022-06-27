use super::*;
use thiserror::Error;

/// Lexer error kinds.
#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum ErrorKind {
    #[error("Mismatched brackets, expected `{0}`, found `{1}`")]
    MismatchedBracs(char, char),

    #[error("Missing opening bracket for `{0}`")]
    MissingOpenBrac(char),

    #[error("Missing closing bracket for `{0}`")]
    MissingCloseBrac(char),

    #[error("Missing digit after numeric literal prefix `{0}`")]
    MissingDigitAfterPrefix(String),

    #[error("Missing closing quote `{0}`")]
    MissingCloseQuote(String),

    #[error("Invalid escape sequence(s) {0:?}")]
    InvalidEscSeq(Vec<String>),
}

/// Lexer error.
pub type Error = Spanned<ErrorKind>;
