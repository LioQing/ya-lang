use super::*;

#[derive(Debug, PartialEq)]
pub enum Item {
    Eof,
    Const(Expr),
}

impl Item {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. })
            if raw.as_str() == "const" => {
                Ok(Item::Const(Expr::Const(ConstExpr::parse(lexer)?)))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Eof, .. }) => {
                lexer.next_token().unwrap();
                Ok(Item::Eof)
            },
            _ => Err(Error::UnknownTokenInGlobalScope { token: lexer.next_token()? }),
        }
    }
}