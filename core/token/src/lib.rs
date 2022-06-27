/// Span of source code.
/// Used to store location of tokens and errors in source code.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Span {
    pub line: usize,
    pub col: std::ops::Range<usize>,
    pub codepoint: std::ops::Range<usize>,
    pub dist_from_prev: usize,
}

impl Span {
    pub fn new(
        line: usize,
        col: std::ops::Range<usize>,
        codepoint: std::ops::Range<usize>,
        dist_from_prev: usize,
    ) -> Self {
        Self { line, col, codepoint, dist_from_prev }
    }
}

/// A wrapper to store any value with span information.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn new_value(value: T) -> Self {
        Self {
            span: Span::new(0, 0..0, 0..0, 0),
            value,
        }
    }
}

impl<T> std::fmt::Display for Spanned<T>
where
    T: std::fmt::Display
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Token kinds.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenKind {
    /** brackets */
    Brac { raw: char, depth: usize, kind: BracKind },

    /** separators */
    Sep { raw: char },

    /** numeric/string/char literals */
    Lit { raw: String, prefix: String, suffix: String, kind: LitKind },

    /** punctuation */
    Punc { raw: String },

    /** identifiers */
    Id { raw: String },

    /** keywords */
    Kw { raw: String }
}

/// Bracket kinds.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BracKind {
    Open,
    Close,
}

/// Literal kinds.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LitKind {
    Integer,
    Float { dot_pos: Option<usize>, exp_pos: Option<usize> },
    Quote { quote: char },
}

/// Token.
pub type Token = Spanned<TokenKind>;
