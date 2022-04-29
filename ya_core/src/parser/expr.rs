use super::*;

#[derive(Debug, PartialEq)]
pub enum ExprOrStmt {
    Expr(Expr),
    Stmt(Stmt),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /** single token expression: directly parsed from 1 token */
    SingleToken(SingleTokenExpr),

    /** block: `{ %[%stmt]* %expr? }` */
    Block(BlockExpr),

    /** parenthesis: `(%expr)` */
    Parens(ParensExpr),

    /** function call: `%expr(%expr)` */
    FuncCall(FuncCallExpr),
}

impl Expr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        // primary expression
        let mut expr = match lexer.peek_token()? {
            lexer::Token::Numeric { .. }
            | lexer::Token::StringChar { .. }
            | lexer::Token::Identifier { .. } => Ok(Expr::SingleToken(SingleTokenExpr::parse(lexer)?)),
            lexer::Token::Bracket { raw: '{', .. } => Ok(Expr::Block(BlockExpr::parse(lexer)?)),
            lexer::Token::Bracket { raw: '(', .. } => Ok(Expr::Parens(ParensExpr::parse(lexer)?)),
            _ => Err(ParserError::ExpectedExpr { found: format!("{:?}", lexer.next_token()?) }),
        }?;

        loop {
            match lexer.peek_token()? {
                lexer::Token::Bracket { raw: '(', .. } => {
                    expr = Expr::FuncCall(FuncCallExpr::parse(lexer, expr)?);
                },
                _ => break Ok(expr),
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SingleTokenExpr {
    Literal(token::Literal),
    VarName(token::VarName),
}

impl SingleTokenExpr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::StringChar { .. } | lexer::Token::Numeric { .. } => {
                Ok(SingleTokenExpr::Literal(token::Literal::parse(lexer)?))
            },
            lexer::Token::Identifier { .. } => {
                Ok(SingleTokenExpr::VarName(token::VarName::parse(lexer)?))
            },
            _ => {
                Err(ParserError::ExpectedPrimExpr { found: format!("{:?}", lexer.next_token()?) })
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

impl BlockExpr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let mut eos = {
            Bracketed::parse(lexer, &[token::Bracket::Curly], |lexer| {
                if let Ok(_) = token::Bracket::peek_parse_close(lexer, &[token::Bracket::Curly]) {
                    return Ok(Separated {
                        separator: Separator::Semicolon,
                        content: vec![],
                    });
                }

                Separated::parse(lexer, Separator::Semicolon, |lexer| {
                    let stmt = Stmt::parse_without_semicolon(lexer)?;
                    match (stmt, Stmt::is_end_of_statement(lexer)) {
                        (stmt, Ok(true)) => {
                            match token::Bracket::peek_nth_parse_close(
                                lexer,
                                &[token::Bracket::Curly],
                                1,
                            ) {
                                Ok(_) => {
                                    Stmt::consume_end_of_statement(lexer).unwrap();
                                    Ok(SepRes::Stop(ExprOrStmt::Stmt(stmt)))
                                },
                                _ => {
                                    Ok(SepRes::Cont(ExprOrStmt::Stmt(stmt)))
                                },
                            }
                        },
                        (Stmt::Expr(expr), Ok(false)) => {
                            Ok(SepRes::Stop(ExprOrStmt::Expr(expr)))
                        },
                        (_, Ok(false)) => {
                            Err(ParserError::ExpectedExpr { found: "statement without semicolon".to_owned() })
                        },
                        (_, Err(err)) => {
                            Err(err)
                        },
                    }
                })
            })
        }?.inner.content;

        let last = eos.pop();

        let mut stmts = eos
            .into_iter()
            .map(|s| {
                if let ExprOrStmt::Stmt(stmt) = s {
                    stmt
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>();

        let expr = match last {
            Some(ExprOrStmt::Expr(expr)) => {
                Some(Box::new(expr))
            },
            Some(ExprOrStmt::Stmt(stmt)) => {
                stmts.push(stmt);
                None
            },
            None => {
                None
            },
        };

        Ok(BlockExpr {
            stmts,
            expr,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct ParensExpr {
    pub expr: Box<Expr>,
}

impl ParensExpr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        Ok(ParensExpr {
            expr: Box::new({
                Bracketed::parse(lexer, &[token::Bracket::Round], |lexer| {
                    Expr::parse(lexer)
                })
            }?.inner),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncCallExpr {
    pub caller: Box<Expr>,
    pub args: Vec<Expr>,
}

impl FuncCallExpr {
    pub fn parse(lexer: &mut lexer::Lexer, caller: Expr) -> Result<Self, ParserError> {
        let args = {
            Bracketed::parse(lexer, &[token::Bracket::Round], |lexer| {
                allow_empty_bracket! {
                    lexer;
                    Separated {
                        separator: Separator::Comma,
                        content: vec![],
                    };
                    token::Bracket::Round
                };

                Separated::parse(lexer, Separator::Comma, |lexer| {
                    let param = Expr::parse(lexer)?;
                    
                    match lexer.peek_token() {
                        Err(_) => {
                            Err(ParserError::Lexer(lexer.next_token().err().unwrap()))
                        },
                        Ok(lexer::Token::Bracket { raw: ')', kind: lexer::BracketKind::Close, .. }) => {
                            Ok(SepRes::Stop(param))
                        },
                        _ => {
                            Ok(SepRes::Cont(param))
                        },
                    }
                })
            })?.inner.content
        };

        Ok(FuncCallExpr {
            caller: Box::new(caller),
            args,
        })
    }
}