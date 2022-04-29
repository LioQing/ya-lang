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

pub enum SepRes<T> {
    Cont { item: T },
    Stop { item: T, is_trailing: bool },
}

macro_rules! separated_helper {
    ($l:ident; $r:expr; $(stop @ $t:pat)?) => {
        let r = $r;
        match $l.peek_token()? {
            $($t => {
                Ok(SepRes::Stop { item: r, is_trailing: false })
            },)?
            _ => {
                Ok(SepRes::Cont { item: r })
            },
        }
    };
    ($l:ident; $r:expr; allow_trailing @ $s:expr; $(stop @ $t:pat)?) => {
        let r = $r;
        match $l.peek_token()? {
            lexer::Token::Separator { raw } if *raw == $s.into() => {
                match $l.peek_nth_token(1)? {
                    $($t => {
                        $l.next_token().unwrap();
                        Ok(SepRes::Stop { item: r, is_trailing: true })
                    },)?
                    _ => {
                        Ok(SepRes::Cont { item: r })
                    },
                }
            },
            $($t => {
                Ok(SepRes::Stop { item: r, is_trailing: false })
            },)?
            _ => {
                Ok(SepRes::Cont { item: r })
            },
        }
    };
}

pub(super) use separated_helper;

macro_rules! separated_parse {
    ($l:ident; $r:expr; $s:expr; $($t:pat)?) => {
        Separated::parse($l, $s, |$l| {
            separated_helper! {
                $l;
                $r;
                stop @ $($t)?
            }
        })
    };
    ($l:ident; $r:expr; $s:expr; allow_trailing; $($t:pat)?) => {
        Separated::parse($l, $s, |$l| {
            separated_helper! {
                $l;
                $r;
                allow_trailing @ $s;
                stop @ $($t)?
            }
        })
    };
    ($l:ident; $r:expr; $s:expr; allow_empty; $($t:pat)?) => {
        while token::Separator::match_token($l.peek_token()?, $s) {
            $l.next_token().unwrap();
        }

        Separated::parse($l, $s, |$l| {
            separated_helper! {
                $l;
                {
                    let r = $r;
                    
                    if token::Separator::match_token($l.peek_token()?, $s) {
                        while token::Separator::match_token($l.peek_nth_token(1)?, $s) {
                            $l.next_token().unwrap();
                        }
                    }

                    r
                };
                stop @ $($t)?
            }
        })
    };
    ($l:ident; $r:expr; $s:expr; allow_empty; allow_trailing; $($t:pat)?) => {
        while token::Separator::match_token($l.peek_token()?, $s) {
            $l.next_token().unwrap();
        }

        Separated::parse($l, $s, |$l| {
            separated_helper! {
                $l;
                {
                    let r = $r;
                    
                    if token::Separator::match_token($l.peek_token()?, $s) {
                        while token::Separator::match_token($l.peek_nth_token(1)?, $s) {
                            $l.next_token().unwrap();
                        }
                    }

                    r
                };
                allow_trailing @ $s;
                stop @ $($t)?
            }
        })
    };
}

pub(super) use separated_parse;

#[derive(Debug, PartialEq)]
pub struct Separated<T> {
    pub separator: token::Separator,
    pub content: Vec<T>,
    pub is_trailing: bool,
}

impl<T> Separated<T> {
    pub fn parse<F>(lexer: &mut lexer::Lexer, sep: token::Separator, parse: F) -> Result<Self, ParserError>
    where
        F: Fn(&mut lexer::Lexer) -> Result<SepRes<T>, ParserError>
    {
        let mut content = Vec::new();

        let is_trailing = loop {
            match parse(lexer)? {
                SepRes::Cont { item } => content.push(item),
                SepRes::Stop { item, is_trailing } => {
                    content.push(item);
                    break is_trailing;
                }
            }

            match lexer.next_token()? {
                t if token::Separator::match_token(&t, sep) => {},
                found => return Err(ParserError::ExpectedSeparator { expected: sep.into(), found }),
            }
        };

        Ok(Separated {
            content,
            separator: sep,
            is_trailing,
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
        token::Operator::parse_with(lexer, &[":"])?;
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
                    separator: token::Separator::Comma,
                    content: vec![],
                    is_trailing: false,
                };
                token::Bracket::Round
            };

            separated_parse! {
                lexer;
                Param::parse(lexer)?;
                token::Separator::Comma;
                allow_trailing;
                lexer::Token::Bracket { raw: ')', .. }
            }
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
        let ret_ty = if token::Operator::peek_parse(lexer, &["->"]).is_ok() {
            token::Operator::parse_with(lexer, &["->"]).unwrap();
            token::TypeName::parse(lexer)?
        } else {
            token::TypeName::PrimType(token::PrimType::Unit)
        };

        Ok(FuncProto { sign, ret_ty })
    }
}