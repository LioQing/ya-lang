use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("
    {
        let mut a: i32 = 10;
        let mut b: i32 = 12;
        a
    }
    ");

    syn.assoc = |prec| {
        if prec == 2 {
            Assoc::Right
        } else {
            Assoc::Left
        }
    };

    syn.rules = rules! {
        // ty

        // 0 % Patt::TyIds, Patt::PuncStr("::"), Patt::Id
        // => |items| {
        //     let mut ty_ids = items[0]
        //         .clone()
        //         .ty_ids_or_err()
        //         .unwrap();
            
        //     ty_ids.value.push(
        //         items[2]
        //             .clone()
        //             .expr_or_err()
        //             .map(|expr| Spanned::new(expr.value.id(), expr.span))
        //     );

        //     StackItem::TyIds(Repeats::new(ty_ids.value, items[0].span().clone()))
        // },

        // paren

        0 % Patt::Brac('('), Patt::Expr, Patt::Brac(')')
        => |items| StackItem::Expr(Expr::new(
            ExprKind::Paren(ParenExpr {
                expr: Box::new(items[1].clone().expr_or_err()),
            }),
            items[0].span().merge(items[2].span()),
        )),

        // stmt & stmts

        0 % Patt::Expr, Patt::PuncStr(";")
        => |items| StackItem::Stmts(Repeats::new(
            RepeatsKind {
                value: items[0]
                    .clone()
                    .expr_or_err()
                    .map(|expr| expr.value)
                    .map_err(|err| err.value),
                next: None,
            },
            items[0].span().merge(items[1].span()),
        )),
        
        0 % Patt::Stmts, Patt::Stmts
        => |items| {
            let mut stmts = items[0]
                .clone()
                .stmts_or_err()
                .unwrap();
            
            stmts.value.push(match items[1]
                .clone()
                .stmts_or_err()
            {
                Ok(stmts) => stmts.value.value
                    .map(|expr| Spanned::new(expr, stmts.span.clone()))
                    .map_err(|err| Spanned::new(err, stmts.span)),
                Err(err) => Err(err),
            });

            StackItem::Stmts(Repeats::new(stmts.value, items[0].span().clone()))
        },

        // block

        0 % Patt::Brac('{'), Patt::Stmts, Patt::Brac('}')
        => |items| StackItem::Expr(Expr::new(
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
        => |items| StackItem::Expr(Expr::new(
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

        // let decl

        2 % Patt::Kw("let"), Patt::Kw("mut"), Patt::Id
        => |items| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: Some(Spanned::new((), items[1].span().clone())),
                id: items[2]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span)),
                ty: None,
            },
            items[0].span().merge(items[2].span()),
        )),

        2 % Patt::Kw("let"), Patt::Id, Patt::PuncStr(":"), Patt::Id
        => |items| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: None,
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span)),
                ty: Some(items[3]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span))
                ),
            },
            items[0].span().merge(items[2].span()),
        )),

        2 % Patt::Kw("let"), Patt::Kw("mut"), Patt::Id, Patt::PuncStr(":"), Patt::Id
        => |items| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: Some(Spanned::new((), items[0].span().clone())),
                id: items[2]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span)),
                ty: Some(items[4]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span)),
                ),
            },
            items[0].span().merge(items[2].span()),
        )),
        
        2 % Patt::Kw("let"), Patt::Id
        => |items| StackItem::LetDecl(LetDecl::new(
            LetDeclKind {
                mutable: None,
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span)),
                ty: None,
            },
            items[0].span().merge(items[1].span()),
        )),

        // let expr
        
        2 % Patt::LetDecl, Patt::PuncStr("="), Patt::Expr
        => |items| items[0]
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
        => |items| items[0]
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
        => |items| StackItem::Expr(Expr::new(
            ExprKind::Const(ConstExpr {
                id: items[1]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span)),
                ty: items[3]
                    .clone()
                    .expr_or_err()
                    .map(|expr| Spanned::new(expr.value.id(), expr.span)),
                expr: Box::new(items[5].clone().expr_or_err()),
            }),
            items[0].span().merge(items[5].span()),
        )),

        // bin op

        0 % Patt::Expr, Patt::OpPunc, Patt::Expr
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