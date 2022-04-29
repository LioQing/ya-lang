use super::*;

#[derive(Debug, PartialEq)]
pub enum Item {
    Eof,
    Func(Func),
}

impl Item {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token() {
            Ok(t) if token::Keyword::match_token(t, &["func"]) => {
                Ok(Item::Func(Func::parse(lexer)?))
            },
            Ok(lexer::Token::Eof) => {
                lexer.next_token().unwrap();
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