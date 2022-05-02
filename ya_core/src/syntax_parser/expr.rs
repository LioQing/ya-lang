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

    /** binary: `$expr $op $expr` */
    Binary(BinaryExpr),

    /** unary: `$op $expr` or `$expr $op` */
    Unary(UnaryExpr),

    /** function: `($[$var_ty_decl,]* $[,]?) $[-> $ty]? $block_expr` */
    Func(FuncExpr),
}

impl Expr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
        // primary expression
        let mut expr = match lexer.peek_token() {
            Ok(lexer::Token::Identifier { raw }) if raw.as_str() == "let" => {
                Ok(Self::Let(LetExpr::parse(lexer)?))
            },
            Ok(lexer::Token::Numeric { .. }) |
            Ok(lexer::Token::StringChar { .. }) => {
                Ok(Self::Lit(token::Lit::parse(lexer)?))
            },
            Ok(lexer::Token::Identifier { .. }) => {
                Ok(Self::VarName(token::VarName::parse(lexer)?))
            },
            Ok(lexer::Token::Bracket { raw: '{', .. }) => {
                Ok(Self::Block(BlockExpr::parse(lexer)?))
            },
            Ok(lexer::Token::Bracket { raw: '(', .. }) => {
                match (lexer.peek_nth_token(1).map(|t| t.clone()), lexer.peek_nth_token(2)) {
                    (
                        Ok(lexer::Token::Bracket { raw: ')', .. }),
                        Ok(lexer::Token::Bracket { raw: '{', .. })
                    ) => {
                        Ok(Self::Func(FuncExpr::parse(lexer)?))
                    },
                    (
                        Ok(lexer::Token::Bracket { raw: ')', .. }),
                        Ok(lexer::Token::Operator { raw, .. })
                    ) if raw.as_str() == "->" => {
                        Ok(Self::Func(FuncExpr::parse(lexer)?))
                    },
                    (Ok(lexer::Token::Bracket { raw: ')', .. }), _) => {
                        Ok(Self::Tuple(TupleExpr::parse(lexer)?))
                    },
                    (
                        Ok(lexer::Token::Identifier { .. }),
                        Ok(lexer::Token::Operator { raw, .. })
                    ) if raw.as_str() == ":" => {
                        Ok(Self::Func(FuncExpr::parse(lexer)?))
                    },
                    _ => {
                        Ok(Self::Tuple(TupleExpr::parse(lexer)?))
                    },
                }
            },
            Ok(lexer::Token::Operator { .. }) => {
                Ok(Self::Unary(UnaryExpr::parse_pre(lexer)?))
            },
            _ => {
                Err(Error::ExpectedExpr { found: format!("{:?}", lexer.next_token()?) })
            },
        }?;

        loop {
            match lexer.peek_token() {
                Ok(lexer::Token::Bracket { raw: '(', .. }) => {
                    expr = Self::Call(CallExpr::parse(lexer, expr)?);
                },
                Ok(lexer::Token::Operator { .. }) => {
                    match lexer.peek_nth_token(1) {
                        Ok(t) if Self::is_expr(t) => {
                            expr = Self::Binary(BinaryExpr::parse(lexer, expr)?);
                        },
                        _ => {
                            expr = Self::Unary(UnaryExpr::parse_post(lexer, expr)?);
                        },
                    }
                },
                Err(_) => break Err(lexer.next_token().err().unwrap().into()),
                _ => break Ok(expr),
            }
        }
    }

    pub fn is_expr(token: &lexer::Token) -> bool {
        match token {
            &lexer::Token::Identifier { ref raw } if raw.as_str() == "let" => true,
            &lexer::Token::Numeric { .. } |
            &lexer::Token::StringChar { .. } => true,
            &lexer::Token::Identifier { .. } => true,
            &lexer::Token::Bracket { raw: '{', .. } => true,
            &lexer::Token::Bracket { raw: '(', .. } => true,
            &lexer::Token::Operator { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}

impl BlockExpr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
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
                    lexer::Token::Bracket { raw: '}', .. }
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
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
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
                lexer::Token::Bracket { raw: ')', .. }
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
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
        token::Keyword::parse(lexer, &["let"])?;
        let var = token::VarName::parse(lexer)?;

        let ty = match lexer.peek_token() {
            Ok(lexer::Token::Operator { raw }) if raw.as_str() == ":" => {
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
    pub fn parse(lexer: &mut lexer::Lexer, caller: Expr) -> Result<Self, Error> {
        let args = TupleExpr::parse(lexer)?.items;

        Ok(Self {
            callee: Box::new(caller),
            args,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub op: token::Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl BinaryExpr {
    pub fn parse(lexer: &mut lexer::Lexer, expr: Expr) -> Result<Self, Error> {
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
pub enum UnaryOpPos {
    Pre,
    Post,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: token::Operator,
    pub op_pos: UnaryOpPos,
    pub expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn parse_pre(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
        let op = token::Operator::parse(lexer)?;
        let expr = Expr::parse(lexer)?;

        Ok(Self {
            op,
            op_pos: UnaryOpPos::Pre,
            expr: Box::new(expr),
        })
    }

    pub fn parse_post(lexer: &mut lexer::Lexer, expr: Expr) -> Result<Self, Error> {
        let op = token::Operator::parse(lexer)?;

        Ok(Self {
            op,
            op_pos: UnaryOpPos::Post,
            expr: Box::new(expr),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncExpr {
    pub params: Vec<VarTypeDecl>,
    pub ret_ty: token::TypeName,
    pub body: Box<BlockExpr>,
}

impl FuncExpr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
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
                lexer::Token::Bracket { raw: ')', .. }
            }
        })?.inner.items;

        // return type
        let ret_ty = match lexer.peek_token() {
            Ok(lexer::Token::Operator { raw }) if raw.as_str() == "->" => {
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