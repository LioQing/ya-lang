use super::*;

macro_rules! err_expr {
    ($($e:expr),*) => {
        Expr::Err(vec![$($e),*])
    };
    ($($e:expr,)*) => {
        Expr::Err(vec![$($e),*])
    };
}

pub(super) use err_expr;

macro_rules! try_expr {
    ($e:expr) => {
        match $e {
            Ok(expr) => expr,
            Err(err) => return err_expr!(err),
        }
    };
}

pub(super) use try_expr;

#[derive(Debug, PartialEq)]
pub enum Expr {
    /** const: const $symbol $[: $type] = $expr */
    Const(ConstExpr),

    /** let: let $symbol $[: $type]? */
    Let(LetExpr),

    /** numeric or string or char literal */
    Lit(token::Lit),

    /** symbol: variable/const names */
    Symbol(token::Symbol),

    /** block: `{ $[$expr$[;]+]* $[$expr]? }` */
    Block(BlockExpr),

    /** tuple: `($[$expr,]* $[,]?)` */
    Tuple(TupleExpr),

    /** call: `$expr $tuple` */
    Call(CallExpr),

    /** binary operation: `$expr $op $expr` */
    BinOp(BinOpExpr),

    /** unary operation: `$op $expr` or `$expr $op` */
    UnOp(UnOpExpr),

    /** function: `($[$var_ty_decl,]* $[,]?) $[-> $ty]? $block_expr` */
    Func(FuncExpr),

    /** error */
    Err(Vec<Error>),
}

impl Expr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Expr {
        let mut expr = Self::parse_prim(lexer);

        let expr = loop {
            match lexer.peek_token() {
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '(', .. }, .. }) => {
                    expr = CallExpr::parse(lexer, expr);
                },
                Err(_) => return err_expr!(lexer.next_token().err().unwrap().into()),
                _ => break expr,
            }
        };
        
        // operators
        match lexer.peek_range_token(0..4).as_slice() {
            &[ // 4 consecutive operators: ambiguous
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
            ] => {
                err_expr!(Error::AmbiguousOperators)
            },
            &[ // 3 consecutive operators: $lhs $suf_un_op $bin_op $pre_un_op $rhs
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                ..
            ] => {
                let lhs_suf_expr = UnOpExpr::parse_suf(lexer, expr);
                BinOpExpr::parse(lexer, lhs_suf_expr)
            },
            &[ // 2 consecutive operators: $lhs$suf_un_op $bin_op $rhs || $lhs $bin_op $pre_un_op$rhs, else ambiguous
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, span }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { span: rhs_span, .. }),
                ..
            ] => match (span.dist_from_prev > 0, rhs_span.dist_from_prev > 0) {
                (true, false) => BinOpExpr::parse(lexer, expr),
                (false, true) => {
                    let lhs_suf_expr = UnOpExpr::parse_suf(lexer, expr);
                    BinOpExpr::parse(lexer, lhs_suf_expr)
                },
                _ => err_expr!(Error::AmbiguousOperators),
            },
            &[ // 1 operator followed by a punctuation or closing bracket token: unary suffix operator
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token {
                    kind: ya_lexer::TokenKind::Operator { .. } |
                    ya_lexer::TokenKind::Separator { .. } |
                    ya_lexer::TokenKind::Bracket { kind: ya_lexer::BracketKind::Close, .. },
                    ..
                }),
                ..
            ] => UnOpExpr::parse_suf(lexer, expr),
            &[ // 1 operator followed by other tokens: binary operator
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                ..
            ] => BinOpExpr::parse(lexer, expr),
            _ => expr,
        }
    }

    fn parse_prim(lexer: &mut ya_lexer::Lexer) -> Expr {
        match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. }) if raw.as_str() == "const" => {
                ConstExpr::parse(lexer)
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. }) if raw.as_str() == "let" => {
                LetExpr::parse(lexer)
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Numeric { .. }, .. }) |
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::StringChar { .. }, .. }) => {
                Self::Lit(try_expr!(token::Lit::parse(lexer)))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { .. }, .. }) => {
                Self::Symbol(try_expr!(token::Symbol::parse(lexer)))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '{', .. }, .. }) => {
                BlockExpr::parse(lexer)
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '(', .. }, .. }) => {
                match (lexer.peek_nth_token(1).map(|t| t.clone()), lexer.peek_nth_token(2)) {
                    (
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }),
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '{', .. }, .. })
                    ) => {
                        FuncExpr::parse(lexer)
                    },
                    (
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }),
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw, .. }, .. })
                    ) if raw.as_str() == "->" => {
                        FuncExpr::parse(lexer)
                    },
                    (Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }), _) => {
                        TupleExpr::parse(lexer)
                    },
                    (
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { .. }, .. }),
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw, .. }, .. })
                    ) if raw.as_str() == ":" => {
                        FuncExpr::parse(lexer)
                    },
                    _ => {
                        TupleExpr::parse(lexer)
                    },
                }
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }) => {
                UnOpExpr::parse_pre(lexer)
            },
            _ => {
                err_expr!(Error::ExpectedExpr { found: format!("{:?}", try_expr!(lexer.next_token().map_err(|e| e.into()))) })
            },
        }
    }
    
    /// find all the expressions satisfying the predicate in the current scope,
    /// i.e. not including expressions inside `{}`, `()`.
    pub fn find_all_curr_scope(&self, pred: fn(&Expr) -> bool) -> Vec<&Expr> {
        if pred(self) {
            vec![self]
        } else {
            match self {
                Self::BinOp(bin) => {
                    let mut v = bin.lhs.find_all_curr_scope(pred);
                    v.append(&mut bin.rhs.find_all_curr_scope(pred));
                    v
                },
                Self::UnOp(un) => un.expr.find_all_curr_scope(pred),
                _ => vec![],
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
    pub sep_errs: Vec<Error>,
}

impl BlockExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Expr {
        let mut seps = try_expr!({
            Bracketed::parse(lexer, &[token::Bracket::Curly], |lexer| {
                allow_empty_bracket! {
                    lexer;
                    Separated::new(token::Separator::Semicolon);
                    token::Bracket::Curly
                };

                Ok(separated_parse! {
                    lexer;
                    Expr::parse(lexer);
                    token::Separator::Semicolon;
                    allow_empty;
                    allow_trailing;
                    ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '}', .. }, .. }
                })
            })
        }).inner;

        let sep_errs = seps.errs;

        let expr = if seps.is_trailing {
            None
        } else if let Some(expr) = seps.items.pop() {
            Some(Box::new(expr))
        } else {
            None
        };

        Expr::Block(Self {
            stmts: seps.items,
            expr,
            sep_errs,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
    pub sep_errs: Vec<Error>,
}

impl TupleExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Expr {
        let sep = try_expr!(Bracketed::parse(lexer, &[token::Bracket::Round],
            |lexer| {
                allow_empty_bracket! {
                    lexer;
                    Separated::new(token::Separator::Comma);
                    token::Bracket::Round
                };
                
                Ok(separated_parse! {
                    lexer;
                    Expr::parse(lexer);
                    token::Separator::Comma;
                    allow_trailing;
                    ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }
                })
            }
        )).inner;

        Expr::Tuple(Self { items: sep.items, sep_errs: sep.errs })
    }
}

#[derive(Debug, PartialEq)]
pub struct ConstExpr {
    pub symbol: token::Symbol,
    pub ty: Option<token::Type>,
    pub expr: Box<Expr>,
}

impl ConstExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Expr {
        try_expr!(token::Keyword::parse(lexer, &["const"]));
        let symbol = try_expr!(token::Symbol::parse(lexer));

        let ty = match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw }, .. }) if raw.as_str() == ":" => {
                try_expr!(lexer.next_token().map_err(|e| e.into()));
                Some(try_expr!(token::Type::parse(lexer)))
            },
            _ => None,
        };

        try_expr!(token::Operator::parse_with(lexer, &["="]));

        let expr = Box::new(Expr::parse(lexer));

        Expr::Const(Self { symbol, ty, expr })
    }
}

#[derive(Debug, PartialEq)]
pub struct LetExpr {
    pub symbol: token::Symbol,
    pub ty: Option<token::Type>,
}

impl LetExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Expr {
        try_expr!(token::Keyword::parse(lexer, &["let"]));
        let symbol = try_expr!(token::Symbol::parse(lexer));

        let ty = match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw }, .. }) if raw.as_str() == ":" => {
                try_expr!(lexer.next_token().map_err(|e| e.into()));
                Some(try_expr!(token::Type::parse(lexer)))
            },
            _ => None,
        };

        Expr::Let(Self { symbol, ty })
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

impl CallExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer, caller: Expr) -> Expr {
        let args = match TupleExpr::parse(lexer) {
            Expr::Tuple(expr) => expr,
            err @ Expr::Err(_) => return err,
            _ => unreachable!(),
        }.items;

        Expr::Call(Self {
            callee: Box::new(caller),
            args,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct BinOpExpr {
    pub op: token::Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl BinOpExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer, expr: Expr) -> Expr {
        let op = try_expr!(token::Operator::parse(lexer));
        let rhs = Expr::parse(lexer);

        Expr::BinOp(Self {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(rhs),
        })
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnOpPos {
    Pre,
    Suf,
}

#[derive(Debug, PartialEq)]
pub struct UnOpExpr {
    pub op: char,
    pub op_pos: UnOpPos,
    pub expr: Box<Expr>,
}

impl UnOpExpr {
    pub fn pre_from_tokens(op: token::Operator, mut expr: Expr) -> Expr {
        if op.op.is_empty() {
            panic!("empty operator");
        }

        for op_char in op.op.chars().rev() {
            expr = Expr::UnOp(Self {
                op: op_char,
                op_pos: UnOpPos::Pre,
                expr: Box::new(expr),
            });
        }

        expr
    }

    pub fn suf_from_tokens(op: token::Operator, mut expr: Expr) -> Expr {
        if op.op.is_empty() {
            panic!("empty operator");
        }

        for op_char in op.op.chars() {
            expr = Expr::UnOp(Self {
                op: op_char,
                op_pos: UnOpPos::Suf,
                expr: Box::new(expr),
            });
        }

        expr
    }

    pub fn parse_pre(lexer: &mut ya_lexer::Lexer) -> Expr {
        let op = try_expr!(token::Operator::parse(lexer));
        let expr = Expr::parse(lexer);

        match expr {
            // if a binary operator follows, this unary operator expression is the lhs of the binary operation
            Expr::BinOp(BinOpExpr { op: bin_op, lhs, rhs }) => {
                Expr::BinOp(BinOpExpr {
                    op: bin_op,
                    lhs: Box::new(Self::pre_from_tokens(op, *lhs)),
                    rhs,
                })
            },
            // if a suffix operator follows, this unary operator expression has higher precedence than the suffix operator
            Expr::UnOp(UnOpExpr { op: suf_op, op_pos: UnOpPos::Suf, mut expr }) => {
                let mut expr_ref = expr.as_mut();
                loop {
                    expr_ref = if let Expr::UnOp(ref mut inner_op) = expr_ref {
                        if let UnOpExpr { op_pos: UnOpPos::Suf, expr: inner_expr, .. } = inner_op {
                            match inner_expr.as_ref() {
                                Expr::UnOp(UnOpExpr { op_pos: UnOpPos::Suf, .. }) => {
                                    inner_expr.as_mut()
                                },
                                _ => {
                                    // temporarily borrow part of the tree
                                    let original = std::mem::replace(inner_expr, Box::new(Expr::Err(vec![])));
                                    
                                    // put back the parsed expression
                                    *inner_expr = Box::new(Self::pre_from_tokens(op, *original));
                                    break;
                                },
                            }
                        } else {
                            unreachable!();
                        }
                    } else {
                        unreachable!();
                    }
                };

                Expr::UnOp(UnOpExpr {
                    op: suf_op,
                    op_pos: UnOpPos::Suf,
                    expr,
                })
            },
            _ => {
                Self::pre_from_tokens(op, expr)
            }
        }
    }

    pub fn parse_suf(lexer: &mut ya_lexer::Lexer, expr: Expr) -> Expr {
        let op = try_expr!(token::Operator::parse(lexer));

        Self::suf_from_tokens(op, expr)
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncExpr {
    pub params: Vec<VarTypeDecl>,
    pub ret_ty: token::Type,
    pub body: Box<Expr>,
    pub sep_errs: Vec<Error>,
}

impl FuncExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Expr {
        // params
        let seps = try_expr!(Bracketed::parse(lexer, &[token::Bracket::Round], |lexer| {
            allow_empty_bracket! {
                lexer;
                Separated {
                    separator: token::Separator::Comma,
                    items: vec![],
                    is_trailing: false,
                    errs: vec![],
                };
                token::Bracket::Round
            };

            Ok(separated_parse! {
                lexer;
                VarTypeDecl::parse(lexer)?;
                token::Separator::Comma;
                allow_trailing;
                ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }
            })
        })).inner;

        let sep_errs = seps.errs;
        let params = seps.items;

        // return type
        let ret_ty = match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw }, .. }) if raw.as_str() == "->" => {
                token::Operator::parse_with(lexer, &["->"]).unwrap();
                try_expr!(token::Type::parse(lexer))
            },
            _ => token::Type::PrimType(PrimType::Unit),
        };

        // body
        let body = BlockExpr::parse(lexer);

        Expr::Func(Self { 
            params,
            ret_ty,
            body: Box::new(body),
            sep_errs,
        })
    }
}