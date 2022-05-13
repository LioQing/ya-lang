use super::*;

#[test]
fn empty_func() {
    let parser = Parser::parse("const main = () {}");

    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        symbol: token::Symbol { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::Type::PrimType(PrimType::Unit),
            body: Box::new(Expr::Block(BlockExpr {
                stmts: vec![],
                expr: None,
                sep_errs: vec![],
            })),
            sep_errs: vec![],
        })),
    })));
}

#[test]
fn hello_world_func() {
    let parser = Parser::parse(r#"
    const hello_world = () {
        println("Hello World!");
    }
    "#);

    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        symbol: token::Symbol { name: "hello_world".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::Type::PrimType(PrimType::Unit),
            body: Box::new(Expr::Block(BlockExpr {
                stmts: vec![
                    Expr::Call(CallExpr {
                        callee: Box::new(Expr::Symbol(token::Symbol { name: "println".to_owned() })),
                        args: vec![
                            Expr::Lit(token::Lit {
                                value: "Hello World!".to_owned(),
                                prefix: "".to_owned(),
                                suffix: "".to_owned(),
                                kind: token::LitKind::String,
                            }),
                        ],
                    }),
                ],
                expr: None,
                sep_errs: vec![],
            })),
            sep_errs: vec![],
        })),
    })));
}

#[test]
fn add_i32_func() {
    let parser = Parser::parse(r#"
    const add = (a: i32, b: i32) -> i32 {
        println(f"Adding {} and {}", a, b);
        a + b
    }
    "#);

    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        symbol: token::Symbol { name: "add".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![
                VarTypeDecl {
                    name: token::Symbol { name: "a".to_owned() },
                    ty: token::Type::PrimType(PrimType::I32),
                },
                VarTypeDecl {
                    name: token::Symbol { name: "b".to_owned() },
                    ty: token::Type::PrimType(PrimType::I32),
                },
            ],
            ret_ty: token::Type::PrimType(PrimType::I32),
            body: Box::new(Expr::Block(BlockExpr {
                stmts: vec![
                    Expr::Call(CallExpr {
                        callee: Box::new(Expr::Symbol(token::Symbol { name: "println".to_owned() })),
                        args: vec![
                            Expr::Lit(token::Lit {
                                value: "Adding {} and {}".to_owned(),
                                prefix: "f".to_owned(),
                                suffix: "".to_owned(),
                                kind: token::LitKind::String,
                            }),
                            Expr::Symbol(token::Symbol { name: "a".to_owned() }),
                            Expr::Symbol(token::Symbol { name: "b".to_owned() }),
                        ],
                    })
                ],
                expr: Some(Box::new(Expr::BinOp(BinOpExpr {
                    op: token::Operator { op: "+".to_owned() },
                    lhs: Box::new(Expr::Symbol(token::Symbol { name: "a".to_owned() })),
                    rhs: Box::new(Expr::Symbol(token::Symbol { name: "b".to_owned() })),
                }))),
                sep_errs: vec![],
            })),
            sep_errs: vec![],
        })),
    })));
}

#[test]
fn extreme_empty_func() {
    let parser = Parser::parse(r#"
    const main = () -> () {
        ;;;;a;;;;
        b
    }
    "#);

    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        symbol: token::Symbol { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::Type::PrimType(PrimType::Unit),
            body: Box::new(Expr::Block(BlockExpr {
                stmts: vec![Expr::Symbol(token::Symbol { name: "a".to_owned() })],
                expr: Some(Box::new(Expr::Symbol(token::Symbol { name: "b".to_owned() }))),
                sep_errs: vec![],
            })),
            sep_errs: vec![],
        })),
    })));
}

#[test]
fn func_params_ret_and_let_expr() {
    let parser = Parser::parse(r#"
    const main = (argc: i32, argv: u8) -> bool {
        let a;
        let b: i32;
        let a = b;
        let a: i32 = b;
    }
    "#);

    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        symbol: token::Symbol { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![
                VarTypeDecl {
                    name: token::Symbol { name: "argc".to_owned() },
                    ty: token::Type::PrimType(PrimType::I32),
                },
                VarTypeDecl {
                    name: token::Symbol { name: "argv".to_owned() },
                    ty: token::Type::PrimType(PrimType::U8),
                },
            ],
            ret_ty: token::Type::PrimType(PrimType::Bool),
            body: Box::new(Expr::Block(BlockExpr {
                stmts: vec![
                    Expr::Let(LetExpr {
                        symbol: token::Symbol { name: "a".to_owned() },
                        ty: None,
                    }),
                    Expr::Let(LetExpr {
                        symbol: token::Symbol { name: "b".to_owned() },
                        ty: Some(token::Type::PrimType(PrimType::I32)),
                    }),
                    Expr::BinOp(BinOpExpr {
                        op: token::Operator { op: "=".to_owned() },
                        lhs: Box::new(Expr::Let(LetExpr {
                            symbol: token::Symbol { name: "a".to_owned() },
                            ty: None,
                        })),
                        rhs: Box::new(Expr::Symbol(token::Symbol { name: "b".to_owned() })),
                    }),
                    Expr::BinOp(BinOpExpr {
                        op: token::Operator { op: "=".to_owned() },
                        lhs: Box::new(Expr::Let(LetExpr {
                            symbol: token::Symbol { name: "a".to_owned() },
                            ty: Some(token::Type::PrimType(PrimType::I32)),
                        })),
                        rhs: Box::new(Expr::Symbol(token::Symbol { name: "b".to_owned() })),
                    }),
                ],
                expr: None,
                sep_errs: vec![],
            })),
            sep_errs: vec![],
        })),
    })));
}

#[test]
fn tuple_and_unit() {
    let parser = Parser::parse(r#"
    const main = () -> (i32, ((), i64)) {
        (a + b, ((), 2i64))
    }
    "#);

    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        symbol: token::Symbol { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::Type::Tuple(vec![
                token::Type::PrimType(PrimType::I32),
                token::Type::Tuple(vec![
                    token::Type::PrimType(PrimType::Unit),
                    token::Type::PrimType(PrimType::I64),
                ]),
            ]),
            body: Box::new(Expr::Block(BlockExpr {
                stmts: vec![],
                expr: Some(Box::new(Expr::Tuple(TupleExpr {
                    items: vec![
                        Expr::BinOp(BinOpExpr {
                            op: token::Operator { op: "+".to_owned() },
                            lhs: Box::new(Expr::Symbol(token::Symbol { name: "a".to_owned() })),
                            rhs: Box::new(Expr::Symbol(token::Symbol { name: "b".to_owned() })),
                        }),
                        Expr::Tuple(TupleExpr {
                            items: vec![
                                Expr::Tuple(TupleExpr { items: vec![], sep_errs: vec![] }),
                                Expr::Lit(token::Lit {
                                    value: "2".to_owned(),
                                    prefix: "".to_owned(),
                                    suffix: "i64".to_owned(),
                                    kind: token::LitKind::Integer,
                                }),
                            ],
                            sep_errs: vec![],
                        }),
                    ],
                    sep_errs: vec![],
                }))),
                sep_errs: vec![],
            })),
            sep_errs: vec![],
        }))
    })));
}

#[test]
fn unary_operators() {
    let parser = Parser::parse(r#"
    const main = () {
        &1- +- +2*;
    
        &1- + -+2*;
    
        &1-+ - +2*;
    
        &1 - +-+2*;
    
        &1-+- + 2*;
    
        &1 + 2*;
        &1+ 2*;
        &1 +2*;
        &1+2*;
    }
    "#);
    
    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        symbol: token::Symbol { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::Type::PrimType(PrimType::Unit),
            body: Box::new(Expr::Block(BlockExpr {
                stmts: vec![
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+-".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '-',
                                            op_pos: UnOpPos::Suf,
                                            expr: Box::new(Expr::Lit(
                                                token::Lit {
                                                    value: "1".to_owned(),
                                                    prefix: "".to_owned(),
                                                    suffix: "".to_owned(),
                                                    kind: token::LitKind::Integer,
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '+',
                                            op_pos: UnOpPos::Pre,
                                            expr: Box::new(Expr::Lit(
                                                token::Lit {
                                                    value: "2".to_owned(),
                                                    prefix: "".to_owned(),
                                                    suffix: "".to_owned(),
                                                    kind: token::LitKind::Integer,
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '-',
                                            op_pos: UnOpPos::Suf,
                                            expr: Box::new(Expr::Lit(
                                                token::Lit {
                                                    value: "1".to_owned(),
                                                    prefix: "".to_owned(),
                                                    suffix: "".to_owned(),
                                                    kind: token::LitKind::Integer,
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '-',
                                            op_pos: UnOpPos::Pre,
                                            expr: Box::new(Expr::UnOp(
                                                UnOpExpr {
                                                    op: '+',
                                                    op_pos: UnOpPos::Pre,
                                                    expr: Box::new(Expr::Lit(
                                                        token::Lit {
                                                            value: "2".to_owned(),
                                                            prefix: "".to_owned(),
                                                            suffix: "".to_owned(),
                                                            kind: token::LitKind::Integer,
                                                        },
                                                    )),
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "-".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '+',
                                            op_pos: UnOpPos::Suf,
                                            expr: Box::new(Expr::UnOp(
                                                UnOpExpr {
                                                    op: '-',
                                                    op_pos: UnOpPos::Suf,
                                                    expr: Box::new(Expr::Lit(
                                                        token::Lit {
                                                            value: "1".to_owned(),
                                                            prefix: "".to_owned(),
                                                            suffix: "".to_owned(),
                                                            kind: token::LitKind::Integer,
                                                        },
                                                    )),
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '+',
                                            op_pos: UnOpPos::Pre,
                                            expr: Box::new(Expr::Lit(
                                                token::Lit {
                                                    value: "2".to_owned(),
                                                    prefix: "".to_owned(),
                                                    suffix: "".to_owned(),
                                                    kind: token::LitKind::Integer,
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "-".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "1".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '+',
                                            op_pos: UnOpPos::Pre,
                                            expr: Box::new(Expr::UnOp(
                                                UnOpExpr {
                                                    op: '-',
                                                    op_pos: UnOpPos::Pre,
                                                    expr: Box::new(Expr::UnOp(
                                                        UnOpExpr {
                                                            op: '+',
                                                            op_pos: UnOpPos::Pre,
                                                            expr: Box::new(Expr::Lit(
                                                                token::Lit {
                                                                    value: "2".to_owned(),
                                                                    prefix: "".to_owned(),
                                                                    suffix: "".to_owned(),
                                                                    kind: token::LitKind::Integer,
                                                                },
                                                            )),
                                                        },
                                                    )),
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '-',
                                            op_pos: UnOpPos::Suf,
                                            expr: Box::new(Expr::UnOp(
                                                UnOpExpr {
                                                    op: '+',
                                                    op_pos: UnOpPos::Suf,
                                                    expr: Box::new(Expr::UnOp(
                                                        UnOpExpr {
                                                            op: '-',
                                                            op_pos: UnOpPos::Suf,
                                                            expr: Box::new(Expr::Lit(
                                                                token::Lit {
                                                                    value: "1".to_owned(),
                                                                    prefix: "".to_owned(),
                                                                    suffix: "".to_owned(),
                                                                    kind: token::LitKind::Integer,
                                                                },
                                                            )),
                                                        },
                                                    )),
                                                },
                                            )),
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "2".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "1".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "2".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "1".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "2".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "1".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "2".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+".to_owned(),
                            },
                            lhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '&',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "1".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                            rhs: Box::new(Expr::UnOp(
                                UnOpExpr {
                                    op: '*',
                                    op_pos: UnOpPos::Suf,
                                    expr: Box::new(Expr::Lit(
                                        token::Lit {
                                            value: "2".to_owned(),
                                            prefix: "".to_owned(),
                                            suffix: "".to_owned(),
                                            kind: token::LitKind::Integer,
                                        },
                                    )),
                                },
                            )),
                        },
                    ),
                ],
                expr: None,
                sep_errs: vec![],
            })),
            sep_errs: vec![],
        }))
    })));
}