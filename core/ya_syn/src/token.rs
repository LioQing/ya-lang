use super::*;

#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq, Clone)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Let,
}

impl Keyword {
    pub fn parse(lexer: &mut ya_lexer::Lexer, keywords: &[&str]) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. }
                if keywords.contains(&raw.as_str()) => Ok(Self::from_str(raw.as_str())?),
            found => Err(Error::ExpectedKeyword {
                expected: keywords.iter().map(|s| s.to_string()).collect(),
                found,
            }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarName {
    pub name: String,
}

impl VarName {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { raw }, .. } => Ok(Self { name: raw }),
            found => return Err(Error::ExpectedIdentifier { found }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    pub ty: Box<TypeName>,
    pub len: Lit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName {
    PrimType(PrimType),
    Struct(String),
    Tuple(Vec<TypeName>),
    Array(Array),
}

impl TypeName {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        match lexer.peek_token()? {
            ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '(', .. }, .. } => {
                let tys = Bracketed::parse(lexer, &[Bracket::Round], |lexer| {
                    allow_empty_bracket! {
                        lexer;
                        Separated::new(token::Separator::Comma);
                        token::Bracket::Round
                    };
        
                    separated_parse! {
                        lexer;
                        Self::parse(lexer)?;
                        token::Separator::Comma;
                        allow_trailing;
                        ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: ')', .. }, .. }
                    }
                })?.inner.items;

                match tys.as_slice() {
                    [] => Ok(Self::PrimType(PrimType::Unit)),
                    [..] => Ok(Self::Tuple(tys)),
                }
            }
            ya_lexer::Token { kind: ya_lexer::TokenKind::Identifier { .. }, .. } => {
                let ty = if let ya_lexer::Token {
                    kind: ya_lexer::TokenKind::Identifier { raw },
                    ..
                } = lexer.next_token().unwrap() {
                    Self::from_str(raw.as_str()).unwrap()
                } else {
                    unreachable!()
                };

                match lexer.peek_token() {
                    Ok(ya_lexer::Token { kind: ya_lexer::TokenKind::Bracket { raw: '[', .. }, .. }) => {
                        let len = Bracketed::parse(lexer, &[Bracket::Square],
                            |lexer| Lit::parse(lexer))?.inner;
                        
                        Ok(TypeName::Array(Array { ty: Box::new(ty), len }))
                    },
                    _ => Ok(ty),
                }
            },
            _ => return Err(Error::ExpectedIdentifier { found: lexer.next_token().unwrap() }),
        }
    }
}

impl std::str::FromStr for TypeName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        if let Ok(prim_type) = PrimType::from_str(s) {
            Ok(Self::PrimType(prim_type))
        } else {
            Ok(Self::Struct(s.to_string()))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Operator {
    pub op: String,
}

impl Operator {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw }, .. } => Ok(Self { op: raw }),
            found => return Err(Error::ExpectedOperator { expected: vec!["[any operator]".to_owned()], found }),
        }
    }

    pub fn parse_with(lexer: &mut ya_lexer::Lexer, ops: &[&str]) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token { kind: ya_lexer::TokenKind::Operator { raw }, .. }
                if ops.contains(&raw.as_str()) => Ok(Self { op: raw }),
            found => Err(Error::ExpectedOperator {
                expected: ops.iter().map(|s| s.to_string()).collect(),
                found,
            }),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Bracket {
    Round,
    Square,
    Curly,
    Angle,
}

impl Bracket {
    pub fn to_open_char(&self) -> char {
        match self {
            Self::Round => '(',
            Self::Square => '[',
            Self::Curly => '{',
            Self::Angle => '<',
        }
    }

    pub fn to_close_char(&self) -> char {
        match self {
            Self::Round => ')',
            Self::Square => ']',
            Self::Curly => '}',
            Self::Angle => '>',
        }
    }

    pub fn from_open_char(c: char) -> Self {
        match c {
            '(' => Self::Round,
            '[' => Self::Square,
            '{' => Self::Curly,
            '<' => Self::Angle,
            _ => panic!("Bracket::from_open_char: unexpected char: {}", c),
        }
    }

    pub fn from_close_char(c: char) -> Self {
        match c {
            ')' => Self::Round,
            ']' => Self::Square,
            '}' => Self::Curly,
            '>' => Self::Angle,
            _ => panic!("Bracket::from_close_char: unexpected char: {}", c),
        }
    }

    pub fn parse_open(lexer: &mut ya_lexer::Lexer, brackets: &[Self]) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token {
                kind: ya_lexer::TokenKind::Bracket {
                    raw,
                    kind: ya_lexer::BracketKind::Open,
                    ..
                },
                ..
            } if brackets.contains(&Self::from_open_char(raw)) => {
                Ok(Self::from_open_char(raw))
            },
            found => Err(Error::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_close(lexer: &mut ya_lexer::Lexer, brackets: &[Bracket]) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token { kind:
                ya_lexer::TokenKind::Bracket {
                    raw,
                    kind: ya_lexer::BracketKind::Close,
                    ..
                },
                ..
            } if brackets.contains(&Self::from_close_char(raw)) => {
                Ok(Self::from_close_char(raw))
            },
            found => Err(Error::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_close_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_open_with_depth(
        lexer: &mut ya_lexer::Lexer,
        brackets: &[Self],
        depth: usize
    ) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token { kind:
                ya_lexer::TokenKind::Bracket {
                    raw,
                    depth: d,
                    kind: ya_lexer::BracketKind::Open,
                },
                ..
            } if brackets.contains(&Self::from_open_char(raw)) && d == depth => {
                Ok(Self::from_open_char(raw))
            },
            found => Err(Error::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_close_with_depth(
        lexer: &mut ya_lexer::Lexer,
        brackets: &[Self],
        depth: usize
    ) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token { kind:
                ya_lexer::TokenKind::Bracket {
                    raw,
                    depth: d,
                    kind: ya_lexer::BracketKind::Close,
                },
                ..
            } if brackets.contains(&Self::from_close_char(raw)) && d == depth => {
                Ok(Self::from_close_char(raw))
            },
            found => Err(Error::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_close_char())
                    .collect(),
                found,
            }),
        }
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
            Self::Comma => ',',
            Self::Semicolon => ';',
        }
    }
}

impl From<char> for Separator {
    fn from(c: char) -> Self {
        match c {
            ',' => Self::Comma,
            ';' => Self::Semicolon,
            _ => panic!("Separator::from: unexpected char: {}", c),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LitKind {
    String,
    Char,
    Integer,
    Float,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lit {
    pub value: String,
    pub prefix: String,
    pub suffix: String,
    pub kind: LitKind,
}

impl Lit {
    pub fn parse(lexer: &mut ya_lexer::Lexer) -> Result<Self, Error> {
        match lexer.next_token()? {
            ya_lexer::Token {
                kind: ya_lexer::TokenKind::StringChar {
                    raw, prefix, suffix,
                    quote: '"'
                },
                ..
            } => {
                Ok(Self { value: raw, prefix, suffix, kind: LitKind::String })
            },
            ya_lexer::Token {
                kind: ya_lexer::TokenKind::StringChar {
                    raw, prefix, suffix,
                    quote: '\''
                },
                ..
            } => {
                Ok(Self { value: raw, prefix, suffix, kind: LitKind::Char })
            },
            ya_lexer::Token {
                kind: ya_lexer::TokenKind::Numeric {
                    raw, prefix, suffix,
                    kind: ya_lexer::NumericKind::Integer
                },
                ..
            } => {
                Ok(Self { value: raw, prefix, suffix, kind: LitKind::Integer })
            },
            ya_lexer::Token {
                kind: ya_lexer::TokenKind::Numeric {
                    raw, prefix, suffix,
                    kind: ya_lexer::NumericKind::Float { .. }
                },
                ..
            } => {
                Ok(Self { value: raw, prefix, suffix, kind: LitKind::Float })
            },
            found => Err(Error::ExpectedLiteral { found }),
        }
    }
}