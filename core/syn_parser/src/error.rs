use thiserror::Error;

/// Syntax error kinds.
#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum ErrorKind {
    #[error("Lexer error: {0}")]
    Lexer(#[from] lexer::ErrorKind),

    #[error("Unknown syntax")]
    UnknownSyntax,
}

/// Syntax error.
pub type Error = token::Spanned<ErrorKind>;
