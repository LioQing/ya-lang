use super::*;

/** precedence of evaluation is in the order of declaration */
#[derive(Debug, PartialEq)]
pub enum Stmt {
    /** no operation: `;` */
    NoOp,

    /** let: let %var_name %[: %type_name]? %[= %expr]; */
    Let(LetStmt),

    /** expression: %expr; */
    Expr(Expr),
}

impl Stmt {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let stmt = Stmt::parse_without_semicolon(lexer)?;
        Stmt::consume_end_of_statement(lexer)?;
        Ok(stmt)
    }

    /** parse without requiring a trailing semicolon */
    pub fn parse_without_semicolon(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token() {
            Ok(lexer::Token::Separator { raw: ';' }) => {
                Ok(Stmt::NoOp)
            },
            Ok(lexer::Token::Identifier { raw }) if raw == token::Keyword::Let.as_ref() => {
                Ok(Stmt::Let(LetStmt::parse(lexer)?))
            },
            Ok(_) => {
                Ok(Stmt::Expr(Expr::parse(lexer)?))
            },
            _ => {
                Err(ParserError::Lexer(lexer.next_token().err().unwrap()))
            },
        }
    }

    pub fn is_end_of_statement(lexer: &mut lexer::Lexer) -> Result<bool, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Separator { raw: ';' } => Ok(true),
            _ => Ok(false),
        }
    }

    pub fn consume_end_of_statement(lexer: &mut lexer::Lexer) -> Result<(), ParserError> {
        match lexer.next_token()? {
            lexer::Token::Separator { raw: ';' } => Ok(()),
            found => Err(ParserError::ExpectedSeparator { expected: ';', found }),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStmt {
    pub var: token::VarName,
    pub ty: Option<token::TypeName>,
    pub expr: Option<Box<Expr>>,
}

impl LetStmt {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        token::Keyword::parse(lexer, &["let"])?;
        let var = token::VarName::parse(lexer)?;

        let ty = if let Ok(_) = token::Operator::peek_parse(lexer, &[":"]) {
            token::Operator::parse(lexer, &[":"]).unwrap();
            Some(token::TypeName::parse(lexer)?)
        } else {
            None
        };

        let expr = if let Ok(_) = token::Operator::peek_parse(lexer, &["="]) {
            token::Operator::parse(lexer, &["="]).unwrap();
            Some(Box::new(Expr::parse(lexer)?))
        } else {
            None
        };

        Ok(LetStmt { var, ty, expr })
    }
}