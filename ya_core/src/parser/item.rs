use super::*;

#[derive(Debug, PartialEq)]
pub enum Item {
    Eof,
    Func(Func),
}

impl Item {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token() {
            Ok(lexer::Token::Identifier { raw }) => {
                match raw.as_str() {
                    raw if token::Keyword::Func.as_ref() == raw => Ok(Item::Func(Func::parse(lexer)?)),
                    _ => Err(ParserError::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
                }
            },
            Ok(lexer::Token::Eof) => {
                lexer.next_token()?;
                Ok(Item::Eof)
            },
            _ => Err(ParserError::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
        }
    }

    /// Parse the declaration of global items.
    /// 
    /// This needed to be called before the actual parsing,
    /// because the parser need to know what items exist
    /// before parsing the codes which uses the items.
    pub fn parse_decl(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token() {
            Ok(lexer::Token::Identifier { raw }) => {
                match raw.as_str() {
                    raw if token::Keyword::Func.as_ref() == raw => Ok(Item::Func(Func::parse_decl(lexer)?)),
                    _ => Err(ParserError::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
                }
            },
            Ok(lexer::Token::Eof) => {
                lexer.next_token()?;
                Ok(Item::Eof)
            },
            _ => Err(ParserError::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub proto: FuncProto,
    pub body: Box<ExprBlock>,
}

impl Func {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let decl = FuncProto::parse(lexer)?;
        let body = ExprBlock::parse(lexer)?;

        Ok(Func { 
            proto: decl,
            body: Box::new(body),
        })
    }

    /// Parse a function declaration with the body parentheses.
    /// 
    /// The body will be set to an empty block.
    pub fn parse_decl(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let decl = FuncProto::parse(lexer)?;

        let body_depth = lexer.curr_bracket_depth();
        Bracketed::parse(lexer, &[token::Bracket::Curly], |lexer| {
            loop {
                match lexer.peek_token()? {
                    lexer::Token::Bracket { raw: '}', depth, .. } if *depth == body_depth => {
                        return Ok(())
                    },
                    lexer::Token::Eof => {
                        lexer.next_token().unwrap();
                        return Err(ParserError::ExpectedBracket { expected: vec!['}'], found: lexer::Token::Eof })
                    },
                    _ => { lexer.next_token()?; },
                }
            }
        })?;

        Ok(Func { 
            proto: decl,
            body: Box::new(ExprBlock { stmts: vec![], expr: None }),
        })
    }
}