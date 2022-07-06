use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("
{
}
3 + 3
    ");

    syn.rules = [
        Rule {
            patt: vec![
                Patt::Expr,
                Patt::PuncStr(";"),
            ],
            prec: 0,
            reduce: |items| items[0]
                .clone()
                .expr_or_err()
                .map(|expr| Stmt::new(
                    expr.value.into(),
                    expr.span.merge(items[1].span()),
                ))
                .into(),
        },
        Rule {
            patt: vec![
                Patt::Stmt,
            ],
            prec: 1,
            reduce: |items| StackItem::Stmts(Stmts::new(
                StmtsKind {
                    expr: items[0]
                        .clone()
                        .stmt_or_err()
                        .map(|stmt| stmt.value.expr),
                    next: None,
                },
                items[0].span().clone(),
            )),
        },
        Rule {
            patt: vec![
                Patt::Stmts,
                Patt::Stmt,
            ],
            prec: 2,
            reduce: |e| {
                let mut stmts = e[0].clone().stmts_or_err().unwrap();

                let mut curr = &mut stmts.value;
                while curr.next.is_some() {
                    curr = &mut curr.next.as_mut().unwrap().as_mut().value;
                }

                curr.next = Some(Box::new(Stmts {
                    value: StmtsKind {
                        expr: e[1].clone().stmt_or_err().map(|stmt| stmt.value.expr), // temporary unwrap, error case impossible
                        next: None,
                    },
                    span: e[1].clone().stmt_or_err().unwrap().span,
                }));

                StackItem::Stmts(Stmts::new(stmts.value, e[0].span().clone()))
            }
        },
        Rule {
            patt: vec![
                Patt::Brac('{'),
                Patt::Stmts,
                Patt::Brac('}'),
            ],
            prec: 0,
            reduce: |e| {
                // turn stmts into a vector of expr
                let stmts = e[1].clone().stmts_or_err().unwrap();
                let mut stmts_vec = vec![];
                let mut curr = stmts;

                while curr.value.next.is_some() {
                    stmts_vec.push(curr.value.expr
                        .map(|expr| Expr::new(expr, curr.span.clone()))
                    );

                    curr = *curr.value.next.unwrap();
                }

                stmts_vec.push(curr.value.expr
                    .map(|expr| Expr::new(expr, curr.span.clone()))
                );

                StackItem::Expr(Expr::new(
                    ExprKind::Block(BlockExpr {
                        stmts: stmts_vec,
                        expr: None,
                    }),
                    e[0].span().merge(e[2].span()),
                ))
            },
        },
        Rule {
            patt: vec![
                Patt::Brac('{'),
                Patt::Stmts,
                Patt::Expr,
                Patt::Brac('}'),
            ],
            prec: 0,
            reduce: |e| {
                // turn stmts into a vector of expr
                let stmts = e[1].clone().stmts_or_err().unwrap();
                let mut stmts_vec = vec![];
                let mut curr = stmts;

                while curr.value.next.is_some() {
                    stmts_vec.push(curr.value.expr
                        .map(|expr| Expr::new(expr, curr.span.clone()))
                    );

                    curr = *curr.value.next.unwrap();
                }

                stmts_vec.push(curr.value.expr
                    .map(|expr| Expr::new(expr, curr.span.clone()))
                );

                StackItem::Expr(Expr::new(
                    ExprKind::Block(BlockExpr {
                        stmts: stmts_vec,
                        expr: Some(Box::new(e[2].clone().expr_or_err())),
                    }),
                    e[0].span().merge(e[3].span()),
                ))
            },
        },
        Rule {
            patt: vec![
                Patt::Brac('{'),
                Patt::Brac('}'),
            ],
            prec: 0,
            reduce: |e| {
                StackItem::Expr(Expr::new(
                    ExprKind::Block(BlockExpr {
                        stmts: vec![],
                        expr: None,
                    }),
                    e[0].span().merge(e[1].span()),
                ))
            },
        },
        Rule {
            patt: vec![
                Patt::Brac('{'),
                Patt::Expr,
                Patt::Brac('}'),
            ],
            prec: 0,
            reduce: |e| {
                StackItem::Expr(Expr::new(
                    ExprKind::Block(BlockExpr {
                        stmts: vec![],
                        expr: Some(Box::new(e[1].clone().expr_or_err())),
                    }),
                    e[0].span().merge(e[1].span()),
                ))
            },
        },
        Rule {
            patt: vec![
                Patt::Expr,
                Patt::Punc,
                Patt::Expr,
            ],
            prec: 0,
            reduce: |e| StackItem::Expr(Expr::new(
                ExprKind::Bin(BinExpr {
                    lhs: Box::new(e[0].clone().expr_or_err()),
                    op: e[1]
                        .clone()
                        .token_or_err()
                        .map(|t| Spanned::<String>::new(
                            t.value.punc(),
                            t.span,
                        )),
                    rhs: Box::new(e[2].clone().expr_or_err()),
                }),
                e[0].span().merge(e[2].span()),
            )),
        },
    ].into();

    syn.parse();

    println!("{:#?}", syn.stack);
}