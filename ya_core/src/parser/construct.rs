use super::*;

#[derive(Debug, PartialEq)]
pub struct Bracketed<T> {
    pub bracket: token::Bracket,
    pub inner: T,
}

impl<T> Bracketed<T> {
    pub fn parse<F>(lexer: &mut lexer::Lexer, brackets: &[token::Bracket], f: F) -> Result<Self, ParserError>
    where
        F: Fn(&mut lexer::Lexer) -> Result<T, ParserError>
    {
        let bracket = token::Bracket::parse_open(lexer, brackets)?;

        let inner = f(lexer)?;

        token::Bracket::parse_close(lexer, &[bracket])?;

        Ok(Bracketed {
            bracket,
            inner,
        })
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Separator {
    Comma,
    Semicolon,
}

impl Into<char> for Separator {
    fn into(self) -> char {
        match self {
            Separator::Comma => ',',
            Separator::Semicolon => ';',
        }
    }
}

impl From<char> for Separator {
    fn from(c: char) -> Self {
        match c {
            ',' => Separator::Comma,
            ';' => Separator::Semicolon,
            _ => panic!("Separator::from: unexpected char: {}", c),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Separated<T> {
    pub separator: Separator,
    pub content: Vec<T>,
}

pub enum SepRes<T> {
    Cont(T),
    Stop(T),
}

impl<T> Separated<T> {
    pub fn parse<F>(lexer: &mut lexer::Lexer, sep: Separator, parse: F) -> Result<Self, ParserError>
    where
        F: Fn(&mut lexer::Lexer) -> Result<SepRes<T>, ParserError>
    {
        let mut content = Vec::new();

        loop {
            match parse(lexer)? {
                SepRes::Cont(c) => content.push(c),
                SepRes::Stop(c) => {
                    content.push(c);
                    break;
                }
            }

            match lexer.next_token()? {
                lexer::Token::Separator { raw } if raw == sep.into() => {},
                found => return Err(ParserError::ExpectedSeparator { expected: sep.into(), found }),
            }
        }

        Ok(Separated {
            content,
            separator: sep,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Param {
    pub name: token::VarName,
    pub ty: token::TypeName,
}

impl Param {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        let name = token::VarName::parse(lexer)?;
        token::Operator::parse(lexer, &[":"])?;
        let ty = token::TypeName::parse(lexer)?;

        Ok(Param { name, ty })
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncSign {
    pub name: token::FuncName,
    pub params: Vec<Param>,
}

impl FuncSign {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        // keyword `func`
        token::Keyword::parse(lexer, &["func"])?;

        // name
        let name = token::FuncName::parse(lexer)?;

        // parameters
        let params = Bracketed::parse(lexer, &[token::Bracket::Round], |lexer| {
            if let Ok(_) = token::Bracket::peek_parse_close(lexer, &[token::Bracket::Round]) {
                return Ok(Separated {
                    separator: Separator::Comma,
                    content: vec![],
                });
            }

            Separated::parse(lexer, Separator::Comma, |lexer| {
                let param = Param::parse(lexer)?;

                match lexer.peek_token() {
                    Err(_) => {
                        Err(ParserError::Lexer(lexer.next_token().err().unwrap()))
                    },
                    Ok(lexer::Token::Bracket { raw: ')', kind: lexer::BracketKind::Close, .. }) => {
                        Ok(SepRes::Stop(param))
                    },
                    _ => {
                        Ok(SepRes::Cont(param))
                    },
                }
            })
        })?.inner.content;

        Ok(FuncSign { name, params })
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncProto {
    pub sign: FuncSign,
    pub ret_ty: token::TypeName,
}

impl FuncProto {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        // sign
        let sign = FuncSign::parse(lexer)?;

        // return type
        let ret_ty = if let Ok(_) = token::Operator::peek_parse(lexer, &["->"]) {
            token::Operator::parse(lexer, &["->"]).unwrap();
            token::TypeName::parse(lexer)?
        } else {
            token::TypeName::PrimType(token::PrimType::Unit)
        };

        Ok(FuncProto { sign, ret_ty })
    }
}