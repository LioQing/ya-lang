use super::*;

#[derive(Debug, PartialEq)]
pub enum Expr {
    /** let: let $var_name $[: $type_name]? */
    Let(LetExpr),

    /** numeric or string or char literal */
    Lit(token::Lit),

    /** variable name */
    VarName(token::VarName),

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
}

impl Expr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        let mut expr = Self::parse_prim(lexer)?;

        let expr = loop {
            match lexer.peek_token() {
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '(', .. }, .. }) => {
                    expr = Self::Call(CallExpr::parse(lexer, expr)?);
                },
                Err(_) => return Err(lexer.next_token().err().unwrap().into()),
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
                Err(Error::AmbiguousOperators)
            },
            &[ // 3 consecutive operators: $lhs $suf_un_op $bin_op $pre_un_op $rhs
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                ..
            ] => {
                let lhs_suf_expr = UnOpExpr::parse_suf(lexer, expr)?;
                Ok(Expr::BinOp(BinOpExpr::parse(lexer, lhs_suf_expr)?))
            },
            &[ // 2 consecutive operators: $lhs$suf_un_op $bin_op $rhs || $lhs $bin_op $pre_un_op$rhs, else ambiguous
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, span }),
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                Ok(ya_lexer::Token { span: rhs_span, .. }),
                ..
            ] => match (span.dist_from_prev > 0, rhs_span.dist_from_prev > 0) {
                (true, false) => Ok(Expr::BinOp(BinOpExpr::parse(lexer, expr)?)),
                (false, true) => {
                    let lhs_suf_expr = UnOpExpr::parse_suf(lexer, expr)?;
                    Ok(Expr::BinOp(BinOpExpr::parse(lexer, lhs_suf_expr)?))
                },
                _ => Err(Error::AmbiguousOperators),
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
            ] => Ok(UnOpExpr::parse_suf(lexer, expr)?),
            &[ // 1 operator followed by other tokens: binary operator
                Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }),
                ..
            ] => Ok(Expr::BinOp(BinOpExpr::parse(lexer, expr)?)),
            _ => Ok(expr),
        }
    }

    pub fn parse_prim(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. }) if raw.as_str() == "let" => {
                Ok(Self::Let(LetExpr::parse(lexer)?))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Numeric { .. }, .. }) |
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::StringChar { .. }, .. }) => {
                Ok(Self::Lit(token::Lit::parse(lexer)?))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { .. }, .. }) => {
                Ok(Self::VarName(token::VarName::parse(lexer)?))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '{', .. }, .. }) => {
                Ok(Self::Block(BlockExpr::parse(lexer)?))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '(', .. }, .. }) => {
                match (lexer.peek_nth_token(1).map(|t| t.clone()), lexer.peek_nth_token(2)) {
                    (
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }),
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '{', .. }, .. })
                    ) => {
                        Ok(Self::Func(FuncExpr::parse(lexer)?))
                    },
                    (
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }),
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw, .. }, .. })
                    ) if raw.as_str() == "->" => {
                        Ok(Self::Func(FuncExpr::parse(lexer)?))
                    },
                    (Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }), _) => {
                        Ok(Self::Tuple(TupleExpr::parse(lexer)?))
                    },
                    (
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { .. }, .. }),
                        Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw, .. }, .. })
                    ) if raw.as_str() == ":" => {
                        Ok(Self::Func(FuncExpr::parse(lexer)?))
                    },
                    _ => {
                        Ok(Self::Tuple(TupleExpr::parse(lexer)?))
                    },
                }
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { .. }, .. }) => {
                Ok(UnOpExpr::parse_pre(lexer)?)
            },
            _ => {
                Err(Error::ExpectedExpr { found: format!("{:?}", lexer.next_token()?) })
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}

impl BlockExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        let mut seps = {
            Bracketed::parse(lexer, &[token::Bracket::Curly], |lexer| {
                allow_empty_bracket! {
                    lexer;
                    Separated::new(token::Separator::Semicolon);
                    token::Bracket::Curly
                }

                separated_parse! {
                    lexer;
                    Expr::parse(lexer)?;
                    token::Separator::Semicolon;
                    allow_empty;
                    allow_trailing;
                    ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '}', .. }, .. }
                }
            })
        }?.inner;

        let expr = if seps.is_trailing {
            None
        } else if let Some(expr) = seps.items.pop() {
            Some(Box::new(expr))
        } else {
            None
        };

        Ok(Self {
            stmts: seps.items,
            expr,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
}

impl TupleExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        let items = Bracketed::parse(lexer, &[token::Bracket::Round], |lexer| {
            allow_empty_bracket! {
                lexer;
                Separated::new(token::Separator::Comma);
                token::Bracket::Round
            };
            
            separated_parse! {
                lexer;
                Expr::parse(lexer)?;
                token::Separator::Comma;
                allow_trailing;
                ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }
            }
        })?.inner.items;

        Ok(Self { items })
    }
}

#[derive(Debug, PartialEq)]
pub struct LetExpr {
    pub var: token::VarName,
    pub ty: Option<token::TypeName>,
}

impl LetExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        token::Keyword::parse(lexer, &["let"])?;
        let var = token::VarName::parse(lexer)?;

        let ty = match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw }, .. }) if raw.as_str() == ":" => {
                lexer.next_token()?;
                Some(token::TypeName::parse(lexer)?)
            },
            _ => None,
        };

        Ok(Self { var, ty })
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

impl CallExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer, caller: Expr) -> Result<Self, Error> {
        let args = TupleExpr::parse(lexer)?.items;

        Ok(Self {
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
    pub fn parse(lexer: &mut ya_lexer::Lexer, expr: Expr) -> Result<Self, Error> {
        let op = token::Operator::parse(lexer)?;
        let rhs = Expr::parse(lexer)?;

        Ok(Self {
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
    pub fn pre_from_tokens(op: token::Operator, mut expr: Expr) -> Self {
        if op.op.len() == 0 {
            panic!("empty operator");
        }

        for op_char in op.op.chars().rev() {
            expr = Expr::UnOp(Self {
                op: op_char,
                op_pos: UnOpPos::Pre,
                expr: Box::new(expr),
            });
        }

        if let Expr::UnOp(expr) = expr {
            expr
        } else {
            unreachable!()
        }
    }

    pub fn suf_from_tokens(op: token::Operator, mut expr: Expr) -> Self {
        if op.op.len() == 0 {
            panic!("empty operator");
        }

        for op_char in op.op.chars() {
            expr = Expr::UnOp(Self {
                op: op_char,
                op_pos: UnOpPos::Suf,
                expr: Box::new(expr),
            });
        }

        if let Expr::UnOp(expr) = expr {
            expr
        } else {
            unreachable!()
        }
    }

    pub fn parse_pre(lexer: &mut ya_lexer::Lexer) -> Result<Expr, Error> {
        let op = token::Operator::parse(lexer)?;
        let expr = Expr::parse(lexer)?;

        match expr {
            Expr::BinOp(BinOpExpr { op: bin_op, lhs, rhs }) => {
                Ok(Expr::BinOp(BinOpExpr {
                    op: bin_op,
                    lhs: Box::new(Expr::UnOp(Self::pre_from_tokens(op, *lhs))),
                    rhs,
                }))
            },
            _ => {
                Ok(Expr::UnOp(Self::pre_from_tokens(op, expr)))
            }
        }
    }

    pub fn parse_suf(lexer: &mut ya_lexer::Lexer, expr: Expr) -> Result<Expr, Error> {
        let op = token::Operator::parse(lexer)?;

        Ok(Expr::UnOp(Self::suf_from_tokens(op, expr)))
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncExpr {
    pub params: Vec<VarTypeDecl>,
    pub ret_ty: token::TypeName,
    pub body: Box<BlockExpr>,
}

impl FuncExpr {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        // params
        let params = Bracketed::parse(lexer, &[token::Bracket::Round], |lexer| {
            allow_empty_bracket! {
                lexer;
                Separated {
                    separator: token::Separator::Comma,
                    items: vec![],
                    is_trailing: false,
                };
                token::Bracket::Round
            };

            separated_parse! {
                lexer;
                VarTypeDecl::parse(lexer)?;
                token::Separator::Comma;
                allow_trailing;
                ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }
            }
        })?.inner.items;

        // return type
        let ret_ty = match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw }, .. }) if raw.as_str() == "->" => {
                token::Operator::parse_with(lexer, &["->"]).unwrap();
                token::TypeName::parse(lexer)?
            },
            _ => token::TypeName::PrimType(PrimType::Unit),
        };

        // body
        let body = BlockExpr::parse(lexer)?;

        Ok(Self { 
            params,
            ret_ty,
            body: Box::new(body),
        })
    }
}