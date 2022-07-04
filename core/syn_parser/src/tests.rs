use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("
{
1 + 1;
2 + 1;
}
    ");

    syn.rules = [
        Rule {
            patt: vec![
                Patt::Expr,
                Patt::PuncStr(";".to_owned()),
            ],
            prec: 0,
            reduce: |e| ReduceResult::Stmt(Stmt::new(
                StmtKind { expr: e[0].clone().expr_or_err().unwrap().value, },
                e[0].span().merge(e[1].span()),
            )),
        },
        Rule {
            patt: vec![
                Patt::Stmt,
            ],
            prec: 0,
            reduce: |e| ReduceResult::Stmts(Stmts::new(
                StmtsKind {
                    expr: e[0].clone().stmt_or_err().unwrap().value.expr, // temporary unwrap, error case impossible
                    next: None,
                },
                e[0].span().clone(),
            )),
        },
        Rule {
            patt: vec![
                Patt::Stmts,
                Patt::Stmt,
            ],
            prec: 1,
            reduce: |e| {
                let mut stmts = e[0].clone().stmts_or_err().unwrap();

                let mut curr = &mut stmts.value;
                while curr.next.is_some() {
                    curr = &mut curr.next.as_mut().unwrap().as_mut().as_mut().unwrap().value;
                }

                curr.next = Some(Box::new(Ok(Stmts {
                    value: StmtsKind {
                        expr: e[1].clone().stmt_or_err().unwrap().value.expr, // temporary unwrap, error case impossible
                        next: None,
                    },
                    span: e[1].clone().stmt_or_err().unwrap().span,
                })));

                ReduceResult::Stmts(Stmts::new(stmts.value, e[0].span().clone()))
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
                    stmts_vec.push(Expr { value: curr.value.expr.clone(), span: curr.span.clone() });
                    curr = curr.value.next.unwrap().unwrap();
                }
                stmts_vec.push(Expr { value: curr.value.expr, span: curr.span });

                ReduceResult::Expr(Expr::new(
                    ExprKind::Block(BlockExpr {
                        stmts: stmts_vec.into_iter().map(|e| Ok(e)).collect(),
                        expr: None,
                    }),
                    e[0].span().merge(e[2].span()),
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
            reduce: |e| ReduceResult::Expr(Expr::new(
                ExprKind::Bin(BinExpr {
                    lhs: Box::new(e[0].clone().expr_or_err()),
                    op: e[1].clone().token_or_err().map(|t| match t {
                        Token { value: TokenKind::Punc(punc), span } => Spanned { value: punc.clone(), span },
                        _ => panic!("not a punc"),
                    }),
                    rhs: Box::new(e[2].clone().expr_or_err()),
                }),
                e[0].span().merge(e[2].span()),
            )),
        },
    ].into();

    syn.parse();

    println!("{:#?}", syn.stack);
}