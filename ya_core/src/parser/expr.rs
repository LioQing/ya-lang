use super::*;

#[derive(Debug, PartialEq)]
pub enum Expr {
    /** let: let $var_name $[: $type_name]? $[= $expr]; */
    Let(LetExpr),

    /** numeric or string or char literal */
    Literal(token::Literal),

    /** variable name */
    VarName(token::VarName),

    /** block: `{ $[$expr$[;]+]* $[$expr]? }` */
    Block(BlockExpr),

    /** parenthesis: `($expr)` */
    Parens(ParensExpr),

    /** function call: `$expr($[$expr,]* $[,]?)` */
    FuncCall(FuncCallExpr),

    /** binary expression: `$expr $op $expr` */
    Binary(BinaryExpr),
}

impl Expr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        // primary expression
        let mut expr = match lexer.peek_token() {
            Ok(lexer::Token::Identifier { raw }) if raw.as_str() == "let" => {
                Ok(Expr::Let(LetExpr::parse(lexer)?))
            },
            Ok(lexer::Token::Numeric { .. }) |
            Ok(lexer::Token::StringChar { .. }) => {
                Ok(Expr::Literal(token::Literal::parse(lexer)?))
            },
            Ok(lexer::Token::Identifier { .. }) => {
                Ok(Expr::VarName(token::VarName::parse(lexer)?))
            },
            Ok(lexer::Token::Bracket { raw: '{', .. }) => {
                Ok(Expr::Block(BlockExpr::parse(lexer)?))
            },
            Ok(lexer::Token::Bracket { raw: '(', .. }) => {
                Ok(Expr::Parens(ParensExpr::parse(lexer)?))
            },
            _ => {
                Err(ParserError::ExpectedExpr { found: format!("{:?}", lexer.next_token()?) })
            },
        }?;

        loop {
            match lexer.peek_token() {
                Ok(lexer::Token::Bracket { raw: '(', .. }) => {
                    expr = Expr::FuncCall(FuncCallExpr::parse(lexer, expr)?);
                },
                Ok(lexer::Token::Operator { .. }) => {
                    expr = Expr::Binary(BinaryExpr::parse(lexer, expr)?);
                },
                Err(_) => break Err(lexer.next_token().err().unwrap().into()),
                _ => break Ok(expr),
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}

impl BlockExpr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let mut separated = {
            Bracketed::parse(lexer, &[token::Bracket::Curly], |lexer| {
                allow_empty_bracket! {
                    lexer;
                    Separated {
                        separator: token::Separator::Semicolon,
                        content: vec![],
                        is_trailing: false,
                    };
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

        let expr = if separated.is_trailing {
            None
        } else if let Some(expr) = separated.content.pop() {
            Some(Box::new(expr))
        } else {
            None
        };

        Ok(BlockExpr {
            stmts: separated.content,
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
pub struct LetExpr {
    pub var: token::VarName,
    pub ty: Option<token::TypeName>,
    pub expr: Option<Box<Expr>>,
}

impl LetExpr {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        token::Keyword::parse(lexer, &["let"])?;
        let var = token::VarName::parse(lexer)?;

        let ty = if token::Operator::peek_parse(lexer, &[":"]).is_ok() {
            token::Operator::parse_with(lexer, &[":"]).unwrap();
            Some(token::TypeName::parse(lexer)?)
        } else {
            None
        };

        let expr = if token::Operator::peek_parse(lexer, &["="]).is_ok() {
            token::Operator::parse_with(lexer, &["="]).unwrap();
            Some(Box::new(Expr::parse(lexer)?))
        } else {
            None
        };

        Ok(LetExpr { var, ty, expr })
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
                        separator: token::Separator::Comma,
                        content: vec![],
                        is_trailing: false,
                    };
                    token::Bracket::Round
                };
                
                separated_parse! {
                    lexer;
                    Expr::parse(lexer)?;
                    token::Separator::Comma;
                    allow_trailing;
                    lexer::Token::Bracket { raw: ')', .. }
                }
            })?.inner.content
        };

        Ok(FuncCallExpr {
            caller: Box::new(caller),
            args,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: token::Operator,
    pub rhs: Box<Expr>,
}

impl BinaryExpr {
    pub fn parse(lexer: &mut lexer::Lexer, expr: Expr) -> Result<Self, ParserError> {
        let op = token::Operator::parse(lexer)?;
        let rhs = Expr::parse(lexer)?;

        Ok(BinaryExpr {
            lhs: Box::new(expr),
            op,
            rhs: Box::new(rhs),
        })
    }
}