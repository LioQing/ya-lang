use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum ErrorKind {
    #[error("Mismatched parentheses, expected `{0}`, found `{1}`")]
    MismatchedParens(char, char),

    #[error("Missing opening parenthesis for `{0}`")]
    MissingOpenParen(char),

    #[error("Missing closing parenthesis for `{0}`")]
    MissingCloseParen(char),

    #[error("Missing digit after numeric literal prefix `{0}`")]
    MissingDigitAfterPrefix(String),

    #[error("Missing closing quote `{0}`")]
    MissingCloseQuote(String),

    #[error("Invalid escape sequence(s) {0:?}")]
    InvalidEscSeq(Vec<String>),
}

pub type Error = token::Spanned<ErrorKind>;
