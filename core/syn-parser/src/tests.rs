use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("
    /* const main =
    () { } */
    ");

    syn.assoc = |prec| {
        if [-1, 2, 3].contains(&prec) {
            Assoc::Right
        } else {
            Assoc::Left
        }
    };

    syn.rules = rules! {
        // ty

        3 % Patt::Id, Patt::PuncStr("::"), Patt::Id
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

        2 % Patt::Kw("let"), Patt::Kw("mut"), Patt::Id
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

        2 % Patt::Kw("let"), Patt::Id, Patt::PuncStr(":"), Patt::Id
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

        2 % Patt::Kw("let"), Patt::Kw("mut"), Patt::Id, Patt::PuncStr(":"), Patt::Id
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
        
        2 % Patt::Kw("let"), Patt::Id
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
        
        2 % Patt::LetDecl, Patt::PuncStr("="), Patt::Expr
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
        
        2 % Patt::LetDecl
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

        2 % Patt::Kw("const"), Patt::Id, Patt::PuncStr(":"), Patt::Id, Patt::PuncStr("="), Patt::Expr
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Const(ConstExpr {
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: Some(items[3]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id()))
                ),
                expr: Box::new(items[5].clone().expr_or_err()),
            }),
            items[0].span().merge(items[5].span()),
        )),

        2 % Patt::Kw("const"), Patt::Id, Patt::PuncStr("="), Patt::Expr
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Const(ConstExpr {
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: None,
                expr: Box::new(items[3].clone().expr_or_err()),
            }),
            items[0].span().merge(items[3].span()),
        )),

        // param decl

        1 % Patt::Kw("mut"), Patt::Id, Patt::PuncStr(":"), Patt::Id
        => |items, _| StackItem::ParamDecl(ParamDecl::new(
            ParamDeclKind {
                mutable: Some(Spanned::new((), items[0].span().clone())),
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: items[3]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
            },
            items[0].span().merge(items[3].span()),
        )),

        0 % Patt::Id, Patt::PuncStr(":"), Patt::Id
        => |items, _| StackItem::ParamDecl(ParamDecl::new(
            ParamDeclKind {
                mutable: None,
                id: items[0]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
                ty: items[2]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id())),
            },
            items[0].span().merge(items[2].span()),
        )),

        // params

        0 % Patt::ParamDecl
        => |items, _| StackItem::Params(Repeats::new(
            RepeatsKind::new(items[0]
                .clone()
                .param_decl_or_err()
                .spanless()
            ),
            items[0].span().clone(),
        )),

        1 % Patt::Params, Patt::PuncStr(","), Patt::ParamDecl
        => |items, _| {
            let mut params = items[0]
                .clone()
                .params_or_err()
                .unwrap();
            
            params.value.push(items[2]
                .clone()
                .param_decl_or_err()
            );

            StackItem::Params(Repeats::new(params.value, items[0].span().clone()))
        },

        // fn decl

        2 % Patt::Brac('('), Patt::Params, Patt::Brac(')')
        => |items, _| StackItem::FnDecl(FnDecl::new(
            FnDeclKind {
                params: items[1]
                    .clone()
                    .params_or_err()
                    .unwrap()
                    .into_value_vec(),
                ret: None,
            },
            items[0].span().merge(items[2].span()),
        )),

        2 % Patt::Brac('('), Patt::Params, Patt::PuncStr(","), Patt::Brac(')')
        => |items, _| StackItem::FnDecl(FnDecl::new(
            FnDeclKind {
                params: items[1]
                    .clone()
                    .params_or_err()
                    .unwrap()
                    .into_value_vec(),
                ret: None,
            },
            items[0].span().merge(items[3].span()),
        )),

        2 % Patt::Brac('('), Patt::Params, Patt::Brac(')'), Patt::PuncStr("->"), Patt::Id
        => |items, _| StackItem::FnDecl(FnDecl::new(
            FnDeclKind {
                params: items[1]
                    .clone()
                    .params_or_err()
                    .unwrap()
                    .into_value_vec(),
                ret: Some(items[4]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id()))
                ),
            },
            items[0].span().merge(items[4].span()),
        )),

        2 %
            Patt::Brac('('), Patt::Params, Patt::PuncStr(","), Patt::Brac(')'),
            Patt::PuncStr("->"), Patt::Id,
        => |items, _| StackItem::FnDecl(FnDecl::new(
            FnDeclKind {
                params: items[1]
                    .clone()
                    .params_or_err()
                    .unwrap()
                    .into_value_vec(),
                ret: Some(items[5]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id()))
                ),
            },
            items[0].span().merge(items[5].span()),
        )),

        2 % Patt::Brac('('), Patt::Brac(')'), Patt::PuncStr("->"), Patt::Id,
        => |items, _| StackItem::FnDecl(FnDecl::new(
            FnDeclKind {
                params: vec![],
                ret: Some(items[3]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.map_value(|expr| expr.id()))
                ),
            },
            items[0].span().merge(items[3].span()),
        )),

        // fn expr

        -2 % Patt::FnDecl, Patt::PuncStr("=>"), Patt::Expr,
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Fn(FnExpr {
                decl: items[0]
                    .clone()
                    .fn_decl_or_err(),
                body: Box::new(items[2]
                    .clone()
                    .expr_or_err()
                ),
            }),
            items[0].span().merge(items[2].span()),
        )),

        -2 % Patt::Brac('('), Patt::Brac(')'), Patt::PuncStr("=>"), Patt::Expr,
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Fn(FnExpr {
                decl: Ok(FnDecl::new(
                    FnDeclKind { params: vec![], ret: None },
                    items[0].span().merge(items[1].span()),
                )),
                body: Box::new(items[3]
                    .clone()
                    .expr_or_err()
                ),
            }),
            items[0].span().merge(items[3].span()),
        )),

        -2 % Patt::FnDecl, Patt::Block,
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Fn(FnExpr {
                decl: items[0]
                    .clone()
                    .fn_decl_or_err(),
                body: Box::new(items[1]
                    .clone()
                    .expr_or_err()
                ),
            }),
            items[0].span().merge(items[1].span()),
        )),

        -2 % Patt::Brac('('), Patt::Brac(')'), Patt::Block,
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Fn(FnExpr {
                decl: Ok(FnDecl::new(
                    FnDeclKind { params: vec![], ret: None },
                    items[0].span().merge(items[1].span()),
                )),
                body: Box::new(items[2]
                    .clone()
                    .expr_or_err()
                ),
            }),
            items[0].span().merge(items[2].span()),
        )),

        // op

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