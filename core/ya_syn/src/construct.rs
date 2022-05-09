use super::*;

macro_rules! allow_empty_bracket {
    ($l:ident; $r:expr; $($b:expr),+) => {
        if let Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw, .. }, .. }) = $l.peek_token() {
            if [$($b,)+].iter().any(|b| b.to_close_char() == *raw) {
                return Ok($r);
            }
        }
    };
    ($l:ident; $r:expr; $($b:expr,)+) => {
        if let Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw, .. }, .. }) = $l.peek_token() {
            if [$($b,)+].iter().any(|b| b.to_close_char() == *raw) {
                return Ok($r);
            }
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
    pub fn parse<F>(lexer: &mut ya_lexer::Lexer, brackets: &[token::Bracket], f: F) -> Result<Self, Error>
    where
        F: Fn(&mut ya_lexer::Lexer) -> Result<T, Error>
    {
        let bracket = token::Bracket::parse_open(lexer, brackets)?;

        let inner = f(lexer)?;

        token::Bracket::parse_close(lexer, &[bracket])?;

        Ok(Self {
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
            &ya_lexer::Token { kind: ya_lexer::TokenKind::Separator { raw }, .. } if raw == $s.into() => {
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
        loop {
            match $l.peek_token()? {
                ya_lexer::Token { kind: ya_lexer::TokenKind::Separator { raw }, .. } if *raw == $s.into() => {
                    $l.next_token().unwrap();
                },
                $($t => {
                    return Ok(Separated {
                        separator: $s,
                        items: vec![],
                        is_trailing: true,
                    })
                },)?
                _ => {
                    break;
                },
            }
        }

        Separated::parse($l, $s, |$l| {
            separated_helper! {
                $l;
                {
                    let r = $r;
                    
                    if matches!(
                        $l.peek_token()?,
                        ya_lexer::Token { kind: ya_lexer::TokenKind::Separator { raw }, .. } if *raw == $s.into()
                     ) {
                        while let ya_lexer::Token { kind: ya_lexer::TokenKind::Separator { raw }, .. } = $l.peek_nth_token(1)? {
                            if *raw == $s.into() {
                                $l.next_token().unwrap();
                            } else {
                                break;
                            }
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
    pub items: Vec<T>,
    pub is_trailing: bool,
}

impl<T> Separated<T> {
    pub fn new(separator: token::Separator) -> Self {
        Self {
            separator,
            items: vec![],
            is_trailing: false,
        }
    }

    pub fn parse<F>(lexer: &mut ya_lexer::Lexer, sep: token::Separator, parse: F) -> Result<Self, Error>
    where
        F: Fn(&mut ya_lexer::Lexer) -> Result<SepRes<T>, Error>
    {
        let mut items = Vec::new();

        let is_trailing = loop {
            match parse(lexer)? {
                SepRes::Cont { item } => items.push(item),
                SepRes::Stop { item, is_trailing } => {
                    items.push(item);
                    break is_trailing;
                }
            }

            match lexer.next_token()? {
                ya_lexer::Token { kind: ya_lexer::TokenKind::Separator { raw }, .. } if raw == sep.into() => {},
                found => return Err(Error::ExpectedSeparator { expected: sep.into(), found }),
            }
        };

        Ok(Self {
            items,
            separator: sep,
            is_trailing,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct VarTypeDecl {
    pub name: token::VarName,
    pub ty: token::TypeName,
}

impl VarTypeDecl {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        let name = token::VarName::parse(lexer)?;
        token::Operator::parse_with(lexer, &[":"])?;
        let ty = token::TypeName::parse(lexer)?;

        Ok(Self { name, ty })
    }
}