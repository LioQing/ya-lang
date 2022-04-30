use super::*;

#[derive(Debug, PartialEq)]
pub enum Item {
    Eof,
    Let(LetExpr),
}

impl Item {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
        match lexer.peek_token() {
            Ok(lexer::Token::Identifier { raw }) if raw.as_str() == "let" => {
                Ok(Item::Let(LetExpr::parse(lexer)?))
            },
            Ok(lexer::Token::Eof) => {
                lexer.next_token().unwrap();
                Ok(Item::Eof)
            },
            _ => Err(Error::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
        }
    }
}