use thiserror::Error;

/// Syntax error kinds.
#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum ErrorKind {
}

/// Syntax error.
pub type Error = token::Spanned<ErrorKind>;
