use super::*;

#[derive(Debug, PartialEq)]
pub enum Item {
    Eof,
    Def(BinOpExpr),
}

impl Item {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, Error> {
        match lexer.peek_token() {
            Ok(lexer::Token { kind: lexer::TokenKind::Identifier { raw }, .. }) if raw.as_str() == "let" => {
                let expr = Expr::Let(LetExpr::parse(lexer)?);
                Ok(Item::Def(BinOpExpr::parse(lexer, expr)?))
            },
            Ok(lexer::Token { kind: lexer::TokenKind::Eof, .. }) => {
                lexer.next_token().unwrap();
                Ok(Item::Eof)
            },
            _ => Err(Error::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
        }
    }
}