use super::*;

#[derive(Debug, PartialEq)]
pub struct Param {
    pub name: token::VarName,
    pub ty: token::TypeName,
}

impl Param {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match (lexer.next_token()?, lexer.next_token()?, lexer.next_token()?) {
            (
                lexer::Token::Identifier { raw: param_name },
                lexer::Token::Operator { raw: op },
                lexer::Token::Identifier { raw: ty },
            ) if op == ":" => {
                Ok(Param {
                    name: token::VarName { name: param_name },
                    ty: token::TypeName::from_str(&ty).unwrap(),
                })
            }
            found => return Err(ParserError::ExpectedParamDecl { found }),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncDecl {
    pub name: token::FuncName,
    pub params: token::Bracketed<token::Separated<Param>>,
    pub ret_ty: token::TypeName,
}

impl FuncDecl {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        // keyword `func`
        token::Keyword::parse(lexer, &["func"])?;

        // name
        let name = token::FuncName::parse(lexer)?;

        // parameters
        let params = token::Bracketed::parse(lexer, &[token::Bracket::Round], 
        |lexer| token::Separated::parse(lexer, token::Separator::Comma,
        |lexer| {
            match lexer.peek_token() {
                Err(_) => (Err(ParserError::Lexer(lexer.next_token().err().unwrap())), false),
                _ => (Param::parse(lexer), match lexer.peek_token() {
                    Err(_) => return (Err(ParserError::Lexer(lexer.next_token().err().unwrap())), false),
                    Ok(lexer::Token::Bracket { raw: ')', kind: lexer::BracketKind::Close, .. }) => false,
                    _ => true,
                }),
            }
        }))?;

        // match lexer.next_token()? {
        //     lexer::Token::Bracket { raw: '(', kind: lexer::BracketKind::Open, .. } => {},
        //     found => return Err(ParserError::ExpectedSymbol { expected: vec!['('.to_string()], found }),
        // }

        // let mut params = Vec::<Param>::new();
        // if let Err(_) | Ok(lexer::Token::Bracket { raw: ')', kind: lexer::BracketKind::Close, .. }) = lexer.peek_token() {
        //     lexer.next_token()?;
        // } else {
        //     loop {
        //         params.push(Param::parse(lexer)?);

        //         match lexer.next_token()? {
        //             lexer::Token::Separator { raw: ',' } => {},
        //             lexer::Token::Bracket { raw: ')', kind: lexer::BracketKind::Close, .. } => break,
        //             found => return Err(ParserError::ExpectedSymbol {
        //                 expected: vec![','.to_string(), ')'.to_string()],
        //                 found
        //             }),
        //         }
        //     }
        // }

        // return type
        let ret_ty = match lexer.peek_token() {
            Ok(lexer::Token::Operator { raw }) if raw.as_str() == "->" => {
                lexer.next_token().unwrap();
                match lexer.next_token()? {
                    lexer::Token::Identifier { raw } => token::TypeName::from_str(&raw).unwrap(),
                    found => return Err(ParserError::ExpectedIdentifier { found }),
                }
            },
            Ok(lexer::Token::Bracket { raw: '{', kind: lexer::BracketKind::Open, .. }) => {
                token::TypeName::PrimType(token::PrimType::Unit)
            },
            _ => {
                return Err(ParserError::ExpectedSymbol {
                    expected: vec!["->".to_owned(), "{".to_owned()],
                    found: lexer.next_token()?
                });
            },
        };

        Ok(FuncDecl {
            name,
            params,
            ret_ty,
        })
    }
}