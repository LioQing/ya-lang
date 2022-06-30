use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("0b101abc + false");

    syn.patts = [
        vec![
            StackItem::Expr(Ok(Expr::new_value(ExprKind::Lit(LitToken::new_kind(LitKind::Int))))),
            StackItem::Expr(Ok(Expr::new_value(ExprKind::Lit(LitToken::new_kind(LitKind::Bool))))),
        ],
        vec![
            StackItem::Expr(Ok(Expr::new_value(ExprKind::Lit(LitToken::new_kind(LitKind::Int))))),
            StackItem::Token(Ok(Token::new_value(TokenKind::Punc("+".to_owned())))),
            StackItem::Expr(Ok(Expr::new_value(ExprKind::Lit(LitToken::new_kind(LitKind::Int))))),
        ],
        vec![
            StackItem::Expr(Ok(Expr::new_value(ExprKind::Lit(LitToken::new_kind(LitKind::Int))))),
            StackItem::Token(Ok(Token::new_value(TokenKind::Punc("+".to_owned())))),
            StackItem::Expr(Ok(Expr::new_value(ExprKind::Lit(LitToken::new_kind(LitKind::Bool))))),
        ],
    ].into();

    syn.parse();
}