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

macro_rules! patt {
    ($($custom_stack_item:ident),* $(,)? $(; op_punc = $ops:literal $(,)?)?) => {
        #[derive(Debug, PartialEq, Eq, Clone, Hash)]
        pub enum Patt {
            $($custom_stack_item,)*

            /** brackets */
            Brac(char),

            /** numeric/string/char literals */
            Lit(LitPatt),

            /** punctuation */
            Punc,

            /** operator punctuations */
            OpPunc,

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
                    $((
                        &Self::$custom_stack_item,
                        &StackItem::$custom_stack_item(_),
                    ))|*
                    | (
                        &Self::Punc,
                        &StackItem::Token(Token { value: TokenKind::Punc(_), .. }),
                    )
                    | (
                        &Self::Id,
                        &StackItem::Expr(Expr { value: ExprKind::Id(_), .. }),
                    ) => true,
                    $((
                        &Self::OpPunc,
                        &StackItem::Token(Token { value: TokenKind::Punc(b), .. }),
                    ) if b.chars().all(|c| $ops.contains(c)) => true,)?
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
                        &StackItem::Expr(Expr { value: ExprKind::Lit(b), .. }),
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
    }
}

patt! {
    Expr,
    Stmts,
    LetDecl,
    TyIds,
    Ty,
    ;
    op_punc = "!@#$%^&*=`?~|/+-<>",
}
