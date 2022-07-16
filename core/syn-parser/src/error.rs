use super::*;
use thiserror::Error;

/// Syntax error kinds.
#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum ErrorKind {
    #[error("Lexer error: {0}")]
    Lexer(#[from] lexer::ErrorKind),

    #[error("Unknown Syntax")]
    UnknownSyntax,
}

/// Syntax error.
pub type Error = token::Spanned<ErrorKind>;

pub type SynResult<T> = Result<T, Error>;

pub trait SynResultSpanlessExt {
    type Success;

    fn spanless(self) -> Result<Self::Success, ErrorKind>;
}

impl<T> SynResultSpanlessExt for SynResult<Spanned<T>> {
    type Success = T;

    fn spanless(self) -> Result<Self::Success, ErrorKind> {
        match self {
            Ok(t) => Ok(t.value),
            Err(e) => Err(e.value),
        }
    }
}

pub trait SynResultWithSpanExt {
    type Success;

    fn into_spanned(self, span: Span) -> SynResult<Spanned<Self::Success>>;
}

impl<T> SynResultWithSpanExt for Result<T, ErrorKind> {
    type Success = T;

    fn into_spanned(self, span: Span) -> SynResult<Spanned<Self::Success>> {
        match self {
            Ok(t) => Ok(Spanned::new(t, span)),
            Err(e) => Err(Spanned::new(e, span)),
        }
    }
}

pub trait SynResultMapSpanExt {
    fn map_span<F>(self, f: F) -> Self
    where
        F: FnMut(Span) -> Span;
}

impl<T> SynResultMapSpanExt for SynResult<Spanned<T>> {
    fn map_span<F>(self, mut f: F) -> Self
    where
        F: FnMut(Span) -> Span
    {
        match self {
            Ok(t) => Ok(Spanned::new(t.value, f(t.span))),
            Err(e) => Err(Error::new(e.value, f(e.span)))
        }
    }
}
