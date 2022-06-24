use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum ErrorKind {
    #[error("Mismatched parentheses, expected `{0}`, found `{1}`")]
    MismatchedParens(char, char),

    #[error("Missing opening parenthesis for `{0}`")]
    MissingOpenParen(char),

    #[error("Missing digit after numeric literal prefix `{0}`")]
    MissingDigitAfterPrefix(String),

    #[error("Missing closing quote `{0}`")]
    MissingCloseQuote(String),

    #[error("Invalid escape sequence(s) {0:?}")]
    InvalidEscapeSequence(Vec<String>),
}

#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: token::Span,
}

impl Error {
    pub fn new(kind: ErrorKind, span: token::Span) -> Self {
        Self { kind, span }
    }

    pub fn new_kind(kind: ErrorKind) -> Self {
        Self {
            kind,
            span: token::Span::new(0, 0..0, 0..0, 0),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
