/// Span of source code.
/// Used to store location of tokens and errors in source code.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Span {
    pub line: usize,
    pub col: usize,
    pub codepoint: std::ops::Range<usize>,
    pub dist_from_prev: usize,
}

impl Span {
    pub fn new(
        line: usize,
        col: usize,
        codepoint: std::ops::Range<usize>,
        dist_from_prev: usize,
    ) -> Self {
        Self { line, col, codepoint, dist_from_prev }
    }

    /// Return a span starting at the first span to the end of the last span.
    pub fn merge(&self, other: &Span) -> Self {
        if self.codepoint.start > other.codepoint.start {
            return other.merge(self);
        }

        Self {
            line: self.line,
            col: self.col,
            codepoint: self.codepoint.start..other.codepoint.end,
            dist_from_prev: self.dist_from_prev,
        }
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
            span: Span::new(0, 0, 0..0, 0),
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
    Brac(BracToken),

    /** numeric/string/char literals */
    Lit(LitToken),

    /** punctuation */
    Punc(String),

    /** identifiers */
    Id(String),

    /** keywords */
    Kw(String),
}

impl TokenKind {
    pub fn brac(self) -> BracToken {
        match self {
            TokenKind::Brac(b) => b,
            _ => panic!("not a bracket token"),
        }
    }

    pub fn lit(self) -> LitToken {
        match self {
            TokenKind::Lit(l) => l,
            _ => panic!("not a literal token"),
        }
    }

    pub fn punc(self) -> String {
        match self {
            TokenKind::Punc(p) => p,
            _ => panic!("not a punctuation token"),
        }
    }

    pub fn id(self) -> String {
        match self {
            TokenKind::Id(i) => i,
            _ => panic!("not an identifier token"),
        }
    }

    pub fn kw(self) -> String {
        match self {
            TokenKind::Kw(k) => k,
            _ => panic!("not a keyword token"),
        }
    }
}

// Bracket token.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct BracToken {
    pub raw: char,
    pub depth: usize,
    pub kind: BracKind,
}

impl BracToken {
    pub fn new(raw: char, kind: BracKind) -> Self {
        Self {
            raw,
            depth: 0,
            kind,
        }
    }
}

/// Bracket kinds.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BracKind {
    Open,
    Close,
}

/// Literal token.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LitToken {
    pub raw: String,
    pub prefix: String,
    pub suffix: String,
    pub kind: LitKind,
}

impl LitToken {
    pub fn new(raw: String, kind: LitKind) -> Self {
        Self {
            raw,
            prefix: String::new(),
            suffix: String::new(),
            kind,
        }
    }

    pub fn new_kind(kind: LitKind) -> Self {
        Self {
            raw: String::new(),
            prefix: String::new(),
            suffix: String::new(),
            kind,
        }
    }
}

/// Literal kinds.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LitKind {
    Int,
    Float { dot_pos: Option<usize>, exp_pos: Option<usize> },
    Quote { quote: char },
    Bool,
}

/// Token.
pub type Token = Spanned<TokenKind>;
