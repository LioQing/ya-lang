use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("
    {
        let mut a: my_scope::MyType = 10;
        a
    }
    ");

    syn.assoc = |prec| {
        if [1, 2].contains(&prec) {
            Assoc::Right
        } else {
            Assoc::Left
        }
    };

    syn.rules = rules! {
        // ty

        2 % Patt::Id, Patt::PuncStr("::"), Patt::Id
        => |items, _| {
            items[2]
                .clone()
                .expr_or_err()
                .map(|expr| expr
                    .map_value(|expr| {
                        let IdExpr { id, scope } = expr.id();

                        ExprKind::Id(IdExpr {
                            id: id,
                            scope: std::iter::once(items[0]
                                    .clone()
                                    .expr_or_err()
                                    .spanless()
                                    .map(|expr| expr.id().id)
                                    .unwrap()
                                )
                                .chain(scope.into_iter())
                                .collect()
                        })
                    })
                    .map_span(|span| span.merge(items[0].span()))
                )
                .into()
        },

        // paren

        0 % Patt::Brac('('), Patt::Expr, Patt::Brac(')')
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Paren(ParenExpr {
                expr: Box::new(items[1].clone().expr_or_err()),
            }),
            items[0].span().merge(items[2].span()),
        )),

        // stmts

        0 % Patt::Expr, Patt::PuncStr(";")
        => |items, _| StackItem::Stmts(Repeats::new(
            RepeatsKind::new(items[0]
                    .clone()
                    .expr_or_err()
                    .spanless(),
            ),
            items[0].span().merge(items[1].span()),
        )),
        
        0 % Patt::Stmts, Patt::Stmts
        => |items, _| {
            let mut stmts = items[0]
                .clone()
                .stmts_or_err()
                .unwrap();
            
            stmts.value.push(match items[1]
                .clone()
                .stmts_or_err()
            {
                Ok(stmts) => stmts.value.value.into_spanned(stmts.span),
                Err(err) => Err(err),
            });

            StackItem::Stmts(Repeats::new(stmts.value, items[0].span().clone()))
        },

        // block

        0 % Patt::Brac('{'), Patt::Stmts, Patt::Brac('}')
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: items[1]
                    .clone()
                    .stmts_or_err()
                    .unwrap()
                    .into_value_vec(),
                expr: None,
            }),
            items[0].span().merge(items[2].span()),
        )),

        0 % Patt::Brac('{'), Patt::Stmts, Patt::Expr, Patt::Brac('}')
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: items[1]
                    .clone()
                    .stmts_or_err()
                    .unwrap()
                    .into_value_vec(),
                expr: Some(Box::new(items[2].clone().expr_or_err())),
            }),
            items[0].span().merge(items[3].span()),
        )),

        0 % Patt::Brac('{'), Patt::Brac('}')
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: vec![],
                expr: None,
            }),
            items[0].span().merge(items[1].span()),
        )),

        0 % Patt::Brac('{'), Patt::Expr, Patt::Brac('}')
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Block(BlockExpr {
                stmts: vec![],
                expr: Some(Box::new(items[1].clone().expr_or_err())),
            }),
            items[0].span().merge(items[1].span()),
        )),

        // let decl

        1 % Patt::Kw("let"), Patt::Kw("mut"), Patt::Id
        => |items, _| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: Some(Spanned::new((), items[1].span().clone())),
                id: items[2]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: None,
            },
            items[0].span().merge(items[2].span()),
        )),

        1 % Patt::Kw("let"), Patt::Id, Patt::PuncStr(":"), Patt::Id
        => |items, _| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: None,
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: Some(items[3]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id()))
                ),
            },
            items[0].span().merge(items[2].span()),
        )),

        1 % Patt::Kw("let"), Patt::Kw("mut"), Patt::Id, Patt::PuncStr(":"), Patt::Id
        => |items, _| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: Some(Spanned::new((), items[0].span().clone())),
                id: items[2]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: Some(items[4]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ),
            },
            items[0].span().merge(items[2].span()),
        )),
        
        1 % Patt::Kw("let"), Patt::Id
        => |items, _| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: None,
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: None,
            },
            items[0].span().merge(items[1].span()),
        )),

        // let expr
        
        1 % Patt::LetDecl, Patt::PuncStr("="), Patt::Expr
        => |items, _| items[0]
            .clone()
            .let_decl_or_err()
            .map(|let_decl| Expr::new(
                ExprKind::Let(LetExpr {
                    mutable: let_decl.value.mutable,
                    id: let_decl.value.id,
                    ty: let_decl.value.ty,
                    expr: Some(Box::new(items[2].clone().expr_or_err())),
                }),
                items[0].span().merge(items[2].span()),
            ))
            .into(),
        
        1 % Patt::LetDecl
        => |items, _| items[0]
            .clone()
            .let_decl_or_err()
            .map(|let_decl| Expr::new(
                ExprKind::Let(LetExpr {
                    mutable: let_decl.value.mutable,
                    id: let_decl.value.id,
                    ty: let_decl.value.ty,
                    expr: None,
                }),
                let_decl.span,
            ))
            .into(),

        // const expr

        1 % Patt::Kw("const"), Patt::Id, Patt::PuncStr(":"), Patt::Id, Patt::PuncStr("="), Patt::Expr
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Const(ConstExpr {
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: items[3]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                expr: Box::new(items[5].clone().expr_or_err()),
            }),
            items[0].span().merge(items[5].span()),
        )),

        // bin op

        0 % Patt::Expr, Patt::OpPunc, Patt::Expr
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Bin(BinExpr {
                lhs: Box::new(items[0].clone().expr_or_err()),
                op: items[1]
                    .clone()
                    .token_or_err()
                    .map(|t| t.map_value(|t| t.punc())),
                rhs: Box::new(items[2].clone().expr_or_err()),
            }),
            items[0].span().merge(items[2].span()),
        )),
    };

    syn.parse();

    println!("{:#?}", syn.stack);
}