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
    pub decl: FuncDecl,
    pub body: Box<Expr>,
}

impl Func {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let decl = FuncDecl::parse(lexer)?;

        // body (ignored)
        let def_depth = match lexer.next_token()? {
            lexer::Token::Bracket { raw: '{', depth, kind: lexer::BracketKind::Open } => depth,
            found => return Err(ParserError::ExpectedSymbol { expected: vec!["{".to_owned()], found }),
        };

        loop {
            match lexer.next_token()? {
                lexer::Token::Bracket { raw: '}', depth, kind: lexer::BracketKind::Close } => {
                    if depth == def_depth {
                        break;
                    }
                },
                found @ lexer::Token::Eof => return Err(ParserError::ExpectedSymbol {
                    expected: vec!["}".to_owned()],
                    found
                }),
                _ => {},
            }
        }

        Ok(Func { 
            decl,
            body: Box::new(Expr { expr_ty: ExprType::NoOp, value_ty: token::TypeName::PrimType(token::PrimType::Unit) }),
        })
    }
}