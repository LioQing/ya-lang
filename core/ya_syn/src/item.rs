use super::*;

#[derive(Debug, PartialEq)]
pub enum Item {
    Eof,
    Const(Expr),
    Err(Error),
}

impl Item {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Self {
        match lexer.peek_token() {
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. })
            if raw.as_str() == "const" => {
                Self::Const(ConstExpr::parse(lexer))
            },
            Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Eof, .. }) => {
                lexer.next_token().unwrap();
                Self::Eof
            },
            _ => Self::Err(Error::UnknownTokenInGlobalScope { token: lexer.next_token().unwrap() }),
        }
    }
}