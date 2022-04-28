use super::*;

#[derive(Debug, PartialEq)]
pub enum ExprOrStmt {
    Expr(Expr),
    Stmt(Stmt),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /** primary expression: expression that can be directly parsed from tokens */
    Prim(PrimExpr),

    /** block: `{ %[%stmt]* %expr? }` */
    Block(ExprBlock),
}

impl Expr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token() {
            Ok(lexer::Token::Bracket { raw: '{', .. }) => {
                Ok(Expr::Block(ExprBlock::parse(lexer)?))
            },
            Ok(token) if PrimExpr::is_prim_expr(token) => {
                Ok(Expr::Prim(PrimExpr::parse(lexer)?))
            }
            _ => {
                Err(ParserError::ExpectedExpr { found: format!("{:?}", lexer.next_token()?) })
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimExpr {
    Literal(token::Literal),
    VarName(token::VarName),
}

impl PrimExpr {
    pub fn is_prim_expr(token: &lexer::Token) -> bool {
        match token {
            lexer::Token::StringChar { .. } |
            lexer::Token::Numeric { .. } | 
            lexer::Token::Identifier { .. } => {
                true
            },
            _ => {
                false
            },
        }
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::StringChar { .. } | lexer::Token::Numeric { .. } => {
                Ok(PrimExpr::Literal(token::Literal::parse(lexer)?))
            },
            lexer::Token::Identifier { .. } => {
                Ok(PrimExpr::VarName(token::VarName::parse(lexer)?))
            },
            _ => {
                Err(ParserError::ExpectedPrimExpr { found: format!("{:?}", lexer.next_token()?) })
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ExprBlock {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

impl ExprBlock {
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
                            let mut lexer_copy = lexer.clone();
                            lexer_copy.skip(1);
                            match token::Bracket::peek_parse_close(&mut lexer_copy, &[token::Bracket::Curly]) {
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

        Ok(ExprBlock {
            stmts,
            expr,
        })
    }
}