use super::*;

#[macro_export]
macro_rules! rules {
    ($($prec:literal % $($patts:expr),+ $(,)? => $reduce:expr),+ $(,)?) => {
        [$(
            Rule {
                patt: vec![$($patts,)+],
                prec: $prec,
                reduce: $reduce,
            },
        )+].into()
    };
}

#[derive(derivative::Derivative, Clone)]
#[derivative(Debug, PartialEq, Eq, Hash)]
pub struct Rule {
    pub patt: Vec<Patt>,
    pub prec: i32,

    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    pub reduce: fn(&[StackItem]) -> StackItem,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Patt {
    /** any expression */
    Expr,

    /** statements */
    Stmts,

    /** statement */
    Stmt,

    /** brackets */
    Brac(char),

    /** numeric/string/char literals */
    Lit(LitPatt),

    /** punctuation */
    Punc,

    /** specific punctuation */
    PuncStr(&'static str),

    /** identifiers */
    Id,

    /** keywords */
    Kw(&'static str),
}

impl Patt {
    pub fn match_item(&self, item: &StackItem) -> bool {
        match (&self, &item) {
            (
                &Self::Expr,
                &StackItem::Expr(_),
            )
            | (
                &Self::Stmts,
                &StackItem::Stmts(_),
            )
            | (
                &Self::Stmt,
                &StackItem::Stmt(_),
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
                &Self::PuncStr(a),
                &StackItem::Token(Token { value: TokenKind::Punc(b), .. }),
            ) if a == &b => true,
            (
                &Self::Brac(a),
                &StackItem::Token(Token { value: TokenKind::Brac(BracToken { raw: b, .. }), .. }),
            ) if a == b => true,
            (
                &Self::Lit(a),
                &StackItem::Token(Token { value: TokenKind::Lit(b), .. }),
            ) if a == b => true,
            (
                &Self::Kw(a),
                &StackItem::Token(Token { value: TokenKind::Kw(b), .. }),
            ) if a == &b => true,
            (_, &StackItem::Err(_)) => true,
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
