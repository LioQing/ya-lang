use super::*;

#[derive(Debug, PartialEq)]
pub enum Item {
    Eof,
    Let(Expr),
}

impl Item {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. }) if raw.as_str() == "let" => {
                let expr = Expr::Let(LetExpr::parse(lexer)?);
                Ok(Item::Let(Expr::BinOp(BinOpExpr::parse(lexer, expr)?)))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Eof, .. }) => {
                lexer.next_token().unwrap();
                Ok(Item::Eof)
            },
            _ => Err(Error::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
        }
    }
}