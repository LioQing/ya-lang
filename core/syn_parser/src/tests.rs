use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("
    {
        1 + 1;
        2
    }
    3 + 3
    ");

    syn.rules = rules! {
        0 % Patt::Expr, Patt::PuncStr(";")
        => |items| items[0]
            .clone()
            .expr_or_err()
            .map(|expr| Stmt::new(
                expr.value.into(),
                expr.span.merge(items[1].span()),
            ))
            .into(),

        1 % Patt::Stmt
        => |items| StackItem::Stmts(Stmts::new(
            StmtsKind {
                expr: items[0]
                    .clone()
                    .stmt_or_err()
                    .map(|stmt| stmt.value.expr)
                    .map_err(|err| err.value),
                next: None,
            },
            items[0].span().clone(),
        )),
        
        2 % Patt::Stmts, Patt::Stmt
        => |items| {
            let mut stmts = items[0]
                .clone()
                .stmts_or_err()
                .unwrap();
            
            stmts.value.push(items[1].clone().stmt_or_err());

            StackItem::Stmts(Stmts::new(stmts.value, items[0].span().clone()))
        },

        0 % Patt::Brac('{'), Patt::Stmts, Patt::Brac('}')
        => |items| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: items[1]
                    .clone()
                    .stmts_or_err()
                    .unwrap()
                    .into_expr_vec(),
                expr: None,
            }),
            items[0].span().merge(items[2].span()),
        )),

        0 % Patt::Brac('{'), Patt::Stmts, Patt::Expr, Patt::Brac('}')
        => |items| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: items[1]
                    .clone()
                    .stmts_or_err()
                    .unwrap()
                    .into_expr_vec(),
                expr: Some(Box::new(items[2].clone().expr_or_err())),
            }),
            items[0].span().merge(items[3].span()),
        )),

        0 % Patt::Brac('{'), Patt::Brac('}')
        => |items| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: vec![],
                expr: None,
            }),
            items[0].span().merge(items[1].span()),
        )),

        0 % Patt::Brac('{'), Patt::Expr, Patt::Brac('}')
        => |items| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: vec![],
                expr: Some(Box::new(items[1].clone().expr_or_err())),
            }),
            items[0].span().merge(items[1].span()),
        )),

        0 % Patt::Expr, Patt::Punc, Patt::Expr
        => |items| StackItem::Expr(Expr::new(
            ExprKind::Bin(BinExpr {
                lhs: Box::new(items[0].clone().expr_or_err()),
                op: items[1]
                    .clone()
                    .token_or_err()
                    .map(|t| Spanned::<String>::new(
                        t.value.punc(),
                        t.span,
                    )),
                rhs: Box::new(items[2].clone().expr_or_err()),
            }),
            items[0].span().merge(items[2].span()),
        )),
    };

    syn.parse();

    println!("{:#?}", syn.stack);
}