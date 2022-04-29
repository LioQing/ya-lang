use super::*;

macro_rules! allow_empty_bracket {
    ($l:ident; $r:expr; $($b:expr),+) => {
        if let Ok(_) = token::Bracket::peek_parse_close($l, &[$($b,)+]) {
            return Ok($r);
        }
    };
    ($l:ident; $r:expr; $($b:expr,)+) => {
        if let Ok(_) = token::Bracket::peek_parse_close($l, &[$($b,)+]) {
            return Ok($r);
        }
    };
}

pub(super) use allow_empty_bracket;

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

pub enum SepRes<T> {
    Cont(T),
    Stop(T),
}

macro_rules! separated_helper {
    ($l:ident; $r:expr; $(stop @ $t:pat)?) => {
        let r = $r;
        match $l.peek_token()? {
            $($t => {
                Ok(SepRes::Stop(r))
            },)?
            _ => {
                Ok(SepRes::Cont(r))
            },
        }
    };
    ($l:ident; $r:expr; trailing @ $s:expr; $(stop @ $t:pat)?) => {
        let r = $r;
        match $l.peek_token()? {
            lexer::Token::Separator { raw } if *raw == $s.into() => {
                match $l.peek_nth_token(1)? {
                    $($t => {
                        $l.next_token().unwrap();
                        Ok(SepRes::Stop(r))
                    },)?
                    _ => {
                        Ok(SepRes::Cont(r))
                    },
                }
            },
            $($t => {
                Ok(SepRes::Stop(r))
            },)?
            _ => {
                Ok(SepRes::Cont(r))
            },
        }
    };
}

pub(super) use separated_helper;

#[derive(Debug, PartialEq)]
pub struct Separated<T> {
    pub separator: Separator,
    pub content: Vec<T>,
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
            allow_empty_bracket! {
                lexer;
                Separated {
                    separator: Separator::Comma,
                    content: vec![],
                };
                token::Bracket::Round
            };

            Separated::parse(lexer, Separator::Comma, |lexer| {
                separated_helper! {
                    lexer;
                    Param::parse(lexer)?;
                    trailing @ Separator::Comma;
                    stop @ lexer::Token::Bracket { raw: ')', kind: lexer::BracketKind::Close, .. }
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