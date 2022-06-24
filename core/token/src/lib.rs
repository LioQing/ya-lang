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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn new_kind(kind: TokenKind) -> Self {
        Self {
            kind,
            span: Span::new(0, 0..0, 0..0, 0),
        }
    }
}
