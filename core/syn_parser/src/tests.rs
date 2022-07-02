use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("(1 + 2 + b)");

    syn.rules = [
        Rule {
            patt: vec![
                Patt::Brac('('),
                Patt::Expr,
                Patt::Brac(')'),
            ],
            to_expr: |e| ExprKind::Paren(ParenExpr {
                expr: Box::new(e[1].clone().expr_or_err()),
            }),
        },
        Rule {
            patt: vec![
                Patt::Expr,
                Patt::Punc,
                Patt::Expr,
            ],
            to_expr: |e| ExprKind::Bin(BinExpr {
                lhs: Box::new(e[0].clone().expr_or_err()),
                op: e[1].clone().token_or_err().map(|t| match t {
                    Token { value: TokenKind::Punc(punc), span } => Spanned { value: punc.clone(), span },
                    _ => panic!("not a punc"),
                }),
                rhs: Box::new(e[2].clone().expr_or_err()),
            }),
        },
    ].into();

    syn.parse();

    println!("{:#?}", syn.stack);
}