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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenKind {
    /** parentheses */
    Paren { raw: char, depth: usize, kind: ParenKind },

    /** separators */
    Sep { raw: char },

    /** numeric/string/char literals */
    Lit { raw: String, prefix: String, suffix: String, kind: LitKind },

    /** punctuation */
    Punc { raw: String },

    /** identifiers */
    Id { raw: String },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ParenKind {
    Open,
    Close,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LitKind {
    Integer,
    Float { dot_pos: Option<usize>, exp_pos: Option<usize> },
    Quote { quote: char },
}

pub type Token = Spanned<TokenKind>;
