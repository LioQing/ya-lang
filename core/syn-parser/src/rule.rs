use super::*;

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

    /// reduction function, takes the matched patterns and next item, returns the reduced stack item
    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    pub reduce: fn(&[StackItem], Option<&StackItem>) -> StackItem,
}

macro_rules! patt {
    (
        $(
            $custom_stack_item:ident
        ),* $(,)? ;
        $(
            $custom_pat:ident @
            $custom_match:pat
        ),* $(,)? =>
        $(op_punc = $ops:literal)?
    ) => {
        #[derive(Debug, PartialEq, Eq, Clone, Hash)]
        pub enum Patt {
            $($custom_stack_item,)*

            $($custom_pat,)*

            /** brackets */
            Brac(char),

            /** any brackets */
            AnyBrac,

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
                    ))|* => true,
                    $((
                        &Self::$custom_pat,
                        $custom_match
                    ))|*
                    | (
                        &Self::Punc,
                        &StackItem::Token(Token { value: TokenKind::Punc(_), .. }),
                    )
                    | (
                        &Self::AnyBrac,
                        &StackItem::Token(Token { value: TokenKind::Brac(_), .. }),
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
    ScopedId,
    ParamDecl,
    Params,
    FnDecl,
    ;
    Block @ StackItem::Expr(Expr { value: ExprKind::Block(_), .. }),
    =>
    op_punc = "!@#$%^&*=`?~|/+-<>"
}

pub fn get_rules() -> HashSet<Rule> {
    fn add_un_op(
        item: &StackItem,
        pre: bool,
        op: &StackItem,
    ) -> SynResult<Expr> {
        let span = item.span().merge(op.span());
        let item = item.clone().expr_or_err();
        let op = op
            .clone()
            .token_or_err()
            .map(|t| t.map_value(|t| t.punc()));

        match item {
            Ok(Expr { value: ExprKind::Un(mut un_expr), .. }) => {
                if pre {
                    un_expr.pre.push(op);
                } else {
                    un_expr.suf.push(op);
                }

                Ok(Expr::new(
                    ExprKind::Un(un_expr),
                    span,
                ))
            },
            expr => {
                let un_expr = if pre {
                    UnExpr {
                        pre: vec![op],
                        suf: vec![],
                        expr: Box::new(expr),
                    }
                } else {
                    UnExpr {
                        pre: vec![],
                        suf: vec![op],
                        expr: Box::new(expr),
                    }
                };

                Ok(Expr::new(
                    ExprKind::Un(un_expr),
                    span,
                ))
            }
        }
    }

    rules! {
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

        0 % Patt::Expr, Patt::OpPunc, Patt::OpPunc, Patt::OpPunc, Patt::OpPunc, Patt::Expr
        => |items, _| StackItem::Err(Error::new(
            ErrorKind::AmbiguousOps,
            items[0].span().merge(items[5].span()),
        )),

        0 % Patt::Expr, Patt::OpPunc, Patt::OpPunc, Patt::OpPunc, Patt::Expr
        => |items, _| StackItem::Expr(Expr::new(
            ExprKind::Bin(BinExpr {
                lhs: Box::new(add_un_op(&items[0], false, &items[1])),
                op: items[2]
                    .clone()
                    .token_or_err()
                    .map(|t| t.map_value(|t| t.punc())),
                rhs: Box::new(add_un_op(&items[4], true, &items[3])),
            }),
            items[0].span().merge(items[4].span()),
        )),

        0 % Patt::Expr, Patt::OpPunc, Patt::OpPunc, Patt::Expr
        => |items, _| match items[1].span().dist_from_prev.cmp(&items[3].span().dist_from_prev) {
            std::cmp::Ordering::Less => StackItem::Expr(Expr::new(
                ExprKind::Bin(BinExpr {
                    lhs: Box::new(add_un_op(&items[0], false, &items[1])),
                    op: items[2]
                        .clone()
                        .token_or_err()
                        .map(|t| t.map_value(|t| t.punc())),
                    rhs: Box::new(items[3].clone().expr_or_err()),
                }),
                items[0].span().merge(items[3].span()),
            )),
            std::cmp::Ordering::Greater => StackItem::Expr(Expr::new(
                ExprKind::Bin(BinExpr {
                    lhs: Box::new(items[0].clone().expr_or_err()),
                    op: items[1]
                        .clone()
                        .token_or_err()
                        .map(|t| t.map_value(|t| t.punc())),
                    rhs: Box::new(add_un_op(&items[3], true, &items[2])),
                }),
                items[0].span().merge(items[3].span()),
            )),
            _ => StackItem::Err(Error::new(
                ErrorKind::AmbiguousOps,
                items[0].span().merge(items[3].span()),
            )),
        },

        0 % Patt::OpPunc, Patt::Expr
        => |items, _| add_un_op(&items[1], true, &items[0]).into(),

        0 % Patt::Expr, Patt::OpPunc
        => |items, next| match next {
            Some(x) if Patt::OpPunc.match_item(x)
                || !Patt::Punc.match_item(x) && !Patt::AnyBrac.match_item(x) =>
            {
                StackItem::None
            },
            _ => match &items[0] {
                StackItem::Expr(Expr { value: ExprKind::Bin(bin), span }) => {
                    let item = bin.rhs.as_ref().clone().into();
                    
                    StackItem::Expr(Expr::new(
                        ExprKind::Bin(BinExpr {
                            rhs: Box::new(add_un_op(
                                &item,
                                false,
                                &items[1]
                            )),
                            ..bin.clone()
                        }),
                        span.clone(),
                    ))
                },
                _ => add_un_op(&items[0], false, &items[1]).into(),
            }
        },
    }
}
