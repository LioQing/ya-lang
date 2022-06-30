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
    Brac(BracToken),

    /** separators */
    Sep(char),

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
    pub fn eq_patt(&self, other: &Self) -> bool {
        use TokenKind::*;
        match (&self, &other) {
            (
                &Brac(BracToken { raw: r1, kind: k1, .. }),
                &Brac(BracToken { raw: r2, kind: k2, .. }),
            ) => r1 == r2 && k1 == k2,
            (&Sep(c1), &Sep(c2)) => c1 == c2,
            (&Lit(l1), &Lit(l2)) => l1.eq_kind(l2),
            (&Kw(k1), &Kw(k2)) => k1 == k2,
            (&Punc(_), &Punc(_))
            | (&Id(_), &Id(_)) => true,
            _ => false,
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

    pub fn eq_kind(&self, other: &Self) -> bool {
        use LitKind::*;
        match (&self.kind, &other.kind) {
            (Int, Int)
            | (Float { .. }, Float { .. })
            | (Quote { .. }, Quote { .. })
            | (Bool, Bool) => true,
            _ => false,
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
