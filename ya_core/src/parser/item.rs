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
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub proto: FuncProto,
    pub body: Box<BlockExpr>,
}

impl Func {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let decl = FuncProto::parse(lexer)?;
        let body = BlockExpr::parse(lexer)?;

        Ok(Func { 
            proto: decl,
            body: Box::new(body),
        })
    }
}