use super::*;

#[derive(derivative::Derivative, Clone)]
#[derivative(Debug, PartialEq, Eq, Hash)]
pub struct Rule {
    pub patt: Vec<Patt>,

    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    pub to_expr: fn(&[StackItem]) -> ExprKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Patt {
    /** any expression */
    Expr,

    /** brackets */
    Brac(char),

    /** separators */
    Sep(char),

    /** numeric/string/char literals */
    Lit(LitPatt),

    /** punctuation */
    Punc,

    /** identifiers */
    Id,

    /** keywords */
    Kw(String),
}

impl Patt {
    pub fn match_item(&self, item: &StackItem) -> bool {
        match (&self, &item) {
            (
                &Self::Expr,
                &StackItem::Expr(_),
            )
            | (
                &Self::Punc,
                &StackItem::Token(Token { value: TokenKind::Punc(_), .. }),
            )
            | (
                &Self::Id,
                &StackItem::Token(Token { value: TokenKind::Id(_), .. }),
            ) => true,
            (
                &Self::Brac(a),
                &StackItem::Token(Token { value: TokenKind::Brac(BracToken { raw: b, .. }), .. }),
            ) if a == b => true,
            (
                &Self::Sep(a),
                &StackItem::Token(Token { value: TokenKind::Sep(b), .. }),
            ) if a == b => true,
            (
                &Self::Lit(a),
                &StackItem::Token(Token { value: TokenKind::Lit(b), .. }),
            ) if a == b => true,
            (
                &Self::Kw(a),
                &StackItem::Token(Token { value: TokenKind::Kw(b), .. }),
            ) if a == b => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LitPatt {
    Int,
    Float,
    Quote,
    Bool,
}

impl PartialEq<LitToken> for LitPatt {
    fn eq(&self, other: &LitToken) -> bool {
        match (self, other) {
            (Self::Int, LitToken { kind: LitKind::Int, .. }) => true,
            (Self::Float, LitToken { kind: LitKind::Float { .. }, .. }) => true,
            (Self::Quote, LitToken { kind: LitKind::Quote { .. }, .. }) => true,
            (Self::Bool, LitToken { kind: LitKind::Bool, .. }) => true,
            _ => false,
        }
    }

    fn ne(&self, other: &LitToken) -> bool {
        !self.eq(other)
    }
}
