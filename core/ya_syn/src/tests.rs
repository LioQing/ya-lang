use super::*;

#[test]
fn empty_func() {
    let parser = Parser::parse("const main = () {}");

    assert_eq!(parser.items.len(), 1);
    assert_eq!(parser.items[0], Item::Const(Expr::Const(ConstExpr {
        ty: None,
        var: token::VarName { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::TypeName::PrimType(PrimType::Unit),
            body: Box::new(BlockExpr {
                stmts: vec![],
                expr: None,
            }),
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
        var: token::VarName { name: "hello_world".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::TypeName::PrimType(PrimType::Unit),
            body: Box::new(BlockExpr {
                stmts: vec![
                    Expr::Call(CallExpr {
                        callee: Box::new(Expr::VarName(token::VarName { name: "println".to_owned() })),
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
            }),
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
        var: token::VarName { name: "add".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![
                VarTypeDecl {
                    name: token::VarName { name: "a".to_owned() },
                    ty: token::TypeName::PrimType(PrimType::I32),
                },
                VarTypeDecl {
                    name: token::VarName { name: "b".to_owned() },
                    ty: token::TypeName::PrimType(PrimType::I32),
                },
            ],
            ret_ty: token::TypeName::PrimType(PrimType::I32),
            body: Box::new(BlockExpr {
                stmts: vec![
                    Expr::Call(CallExpr {
                        callee: Box::new(Expr::VarName(token::VarName { name: "println".to_owned() })),
                        args: vec![
                            Expr::Lit(token::Lit {
                                value: "Adding {} and {}".to_owned(),
                                prefix: "f".to_owned(),
                                suffix: "".to_owned(),
                                kind: token::LitKind::String,
                            }),
                            Expr::VarName(token::VarName { name: "a".to_owned() }),
                            Expr::VarName(token::VarName { name: "b".to_owned() }),
                        ],
                    })
                ],
                expr: Some(Box::new(Expr::BinOp(BinOpExpr {
                    op: token::Operator { op: "+".to_owned() },
                    lhs: Box::new(Expr::VarName(token::VarName { name: "a".to_owned() })),
                    rhs: Box::new(Expr::VarName(token::VarName { name: "b".to_owned() })),
                }))),
            }),
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
        var: token::VarName { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::TypeName::PrimType(PrimType::Unit),
            body: Box::new(BlockExpr {
                stmts: vec![Expr::VarName(token::VarName { name: "a".to_owned() })],
                expr: Some(Box::new(Expr::VarName(token::VarName { name: "b".to_owned() }))),
            }),
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
        var: token::VarName { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![
                VarTypeDecl {
                    name: token::VarName { name: "argc".to_owned() },
                    ty: token::TypeName::PrimType(PrimType::I32),
                },
                VarTypeDecl {
                    name: token::VarName { name: "argv".to_owned() },
                    ty: token::TypeName::PrimType(PrimType::U8),
                },
            ],
            ret_ty: token::TypeName::PrimType(PrimType::Bool),
            body: Box::new(BlockExpr {
                stmts: vec![
                    Expr::Let(LetExpr {
                        var: token::VarName { name: "a".to_owned() },
                        ty: None,
                    }),
                    Expr::Let(LetExpr {
                        var: token::VarName { name: "b".to_owned() },
                        ty: Some(token::TypeName::PrimType(PrimType::I32)),
                    }),
                    Expr::BinOp(BinOpExpr {
                        op: token::Operator { op: "=".to_owned() },
                        lhs: Box::new(Expr::Let(LetExpr {
                            var: token::VarName { name: "a".to_owned() },
                            ty: None,
                        })),
                        rhs: Box::new(Expr::VarName(token::VarName { name: "b".to_owned() })),
                    }),
                    Expr::BinOp(BinOpExpr {
                        op: token::Operator { op: "=".to_owned() },
                        lhs: Box::new(Expr::Let(LetExpr {
                            var: token::VarName { name: "a".to_owned() },
                            ty: Some(token::TypeName::PrimType(PrimType::I32)),
                        })),
                        rhs: Box::new(Expr::VarName(token::VarName { name: "b".to_owned() })),
                    }),
                ],
                expr: None,
            }),
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
        var: token::VarName { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::TypeName::Tuple(vec![
                token::TypeName::PrimType(PrimType::I32),
                token::TypeName::Tuple(vec![
                    token::TypeName::PrimType(PrimType::Unit),
                    token::TypeName::PrimType(PrimType::I64),
                ]),
            ]),
            body: Box::new(BlockExpr {
                stmts: vec![],
                expr: Some(Box::new(Expr::Tuple(TupleExpr {
                    items: vec![
                        Expr::BinOp(BinOpExpr {
                            op: token::Operator { op: "+".to_owned() },
                            lhs: Box::new(Expr::VarName(token::VarName { name: "a".to_owned() })),
                            rhs: Box::new(Expr::VarName(token::VarName { name: "b".to_owned() })),
                        }),
                        Expr::Tuple(TupleExpr {
                            items: vec![
                                Expr::Tuple(TupleExpr { items: vec![] }),
                                Expr::Lit(token::Lit {
                                    value: "2".to_owned(),
                                    prefix: "".to_owned(),
                                    suffix: "i64".to_owned(),
                                    kind: token::LitKind::Integer,
                                }),
                            ],
                        }),
                    ],
                }))),
            }),
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
        var: token::VarName { name: "main".to_owned() },
        expr: Box::new(Expr::Func(FuncExpr {
            params: vec![],
            ret_ty: token::TypeName::PrimType(PrimType::Unit),
            body: Box::new(BlockExpr {
                stmts: vec![
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+-".to_owned()
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
                                    op: '+',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::UnOp(
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
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "+".to_owned()
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
                                    op: '-',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::UnOp(
                                        UnOpExpr {
                                            op: '+',
                                            op_pos: UnOpPos::Pre,
                                            expr: Box::new(Expr::UnOp(
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
                                    )),
                                },
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "-".to_owned()
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
                                    op: '+',
                                    op_pos: UnOpPos::Pre,
                                    expr: Box::new(Expr::UnOp(
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
                            )),
                        },
                    ),
                    Expr::BinOp(
                        BinOpExpr {
                            op: token::Operator {
                                op: "-".to_owned()
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
                                                    expr: Box::new(Expr::UnOp(
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
                                op: "+".to_owned()
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
                                op: "+".to_owned()
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
                                op: "+".to_owned()
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
                                op: "+".to_owned()
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
            }),
        }))
    })));
}