use super::*;

#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq, Clone)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Func,
    Let,
}

impl Keyword {
    pub fn peek_parse(lexer: &mut lexer::Lexer, keywords: &[&str]) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Identifier { raw } if keywords.contains(&raw.as_str()) => Ok(Keyword::from_str(raw.as_str())?),
            found => Err(ParserError::ExpectedKeyword {
                expected: keywords.iter().map(|s| s.to_string()).collect(),
                found: found.clone(),
            }),
        }
    }

    pub fn parse(lexer: &mut lexer::Lexer, keywords: &[&str]) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } if keywords.contains(&raw.as_str()) => Ok(Keyword::from_str(&raw)?),
            found => Err(ParserError::ExpectedKeyword {
                expected: keywords.iter().map(|s| s.to_string()).collect(),
                found,
            }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncName {
    pub name: String,
}

impl FuncName {
    pub fn peek_parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Identifier { raw } => Ok(FuncName { name: raw.clone() }),
            found => return Err(ParserError::ExpectedIdentifier { found: found.clone()}),
        }
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } => Ok(FuncName { name: raw }),
            found => return Err(ParserError::ExpectedIdentifier { found }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarName {
    pub name: String,
}

impl VarName {
    pub fn peek_parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Identifier { raw } => Ok(VarName { name: raw.clone() }),
            found => return Err(ParserError::ExpectedIdentifier { found: found.clone()}),
        }
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } => Ok(VarName { name: raw }),
            found => return Err(ParserError::ExpectedIdentifier { found }),
        }
    }
}

#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum PrimType {
    Unit,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName {
    PrimType(PrimType),
    Struct(String),
}

impl TypeName {
    pub fn peek_parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Identifier { raw } => Ok(TypeName::from_str(&raw).unwrap()),
            found => return Err(ParserError::ExpectedIdentifier { found: found.clone() }),
        }
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } => Ok(TypeName::from_str(&raw).unwrap()),
            found => return Err(ParserError::ExpectedIdentifier { found }),
        }
    }
}

impl std::str::FromStr for TypeName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        if let Ok(prim_type) = PrimType::from_str(s) {
            Ok(TypeName::PrimType(prim_type))
        } else {
            Ok(TypeName::Struct(s.to_string()))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Operator {
    pub op: String,
}

impl Operator {
    pub fn peek_parse(lexer: &mut lexer::Lexer, ops: &[&str]) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Operator { raw } if ops.contains(&raw.as_str()) => Ok(Operator { op: raw.clone() }),
            found => Err(ParserError::ExpectedOperator {
                expected: ops.iter().map(|s| s.to_string()).collect(),
                found: found.clone(),
            }),
        }
    }

    pub fn parse(lexer: &mut lexer::Lexer, ops: &[&str]) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Operator { raw } if ops.contains(&raw.as_str()) => Ok(Operator { op: raw }),
            found => Err(ParserError::ExpectedOperator {
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
            Bracket::Round => '(',
            Bracket::Square => '[',
            Bracket::Curly => '{',
            Bracket::Angle => '<',
        }
    }

    pub fn to_close_char(&self) -> char {
        match self {
            Bracket::Round => ')',
            Bracket::Square => ']',
            Bracket::Curly => '}',
            Bracket::Angle => '>',
        }
    }

    pub fn from_open_char(c: char) -> Self {
        match c {
            '(' => Bracket::Round,
            '[' => Bracket::Square,
            '{' => Bracket::Curly,
            '<' => Bracket::Angle,
            _ => panic!("Bracket::from_open_char: unexpected char: {}", c),
        }
    }

    pub fn from_close_char(c: char) -> Self {
        match c {
            ')' => Bracket::Round,
            ']' => Bracket::Square,
            '}' => Bracket::Curly,
            '>' => Bracket::Angle,
            _ => panic!("Bracket::from_close_char: unexpected char: {}", c),
        }
    }

    pub fn peek_parse_open(lexer: &mut lexer::Lexer, brackets: &[Bracket]) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Bracket {
                raw,
                kind: lexer::BracketKind::Open,
                ..
            } if brackets.contains(&Bracket::from_open_char(*raw)) => {
                Ok(Bracket::from_open_char(*raw))
            },
            found => Err(ParserError::ExpectedSymbol {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char().to_string())
                    .collect(),
                found: found.clone(),
            }),
        }
    }

    pub fn peek_parse_close(lexer: &mut lexer::Lexer, brackets: &[Bracket]) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Bracket {
                raw,
                kind: lexer::BracketKind::Close,
                ..
            } if brackets.contains(&Bracket::from_close_char(*raw)) => {
                Ok(Bracket::from_close_char(*raw))
            },
            found => Err(ParserError::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found: found.clone(),
            }),
        }
    }

    pub fn parse_open(lexer: &mut lexer::Lexer, brackets: &[Bracket]) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Bracket {
                raw,
                kind: lexer::BracketKind::Open,
                ..
            } if brackets.contains(&Bracket::from_open_char(raw)) => {
                Ok(Bracket::from_open_char(raw))
            },
            found => Err(ParserError::ExpectedSymbol {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char().to_string())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_close(lexer: &mut lexer::Lexer, brackets: &[Bracket]) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Bracket {
                raw,
                kind: lexer::BracketKind::Close,
                ..
            } if brackets.contains(&Bracket::from_close_char(raw)) => {
                Ok(Bracket::from_close_char(raw))
            },
            found => Err(ParserError::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn peek_parse_open_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Bracket {
                raw,
                depth: d,
                kind: lexer::BracketKind::Open,
            } if brackets.contains(&Bracket::from_open_char(*raw)) && *d == depth => {
                Ok(Bracket::from_open_char(*raw))
            },
            found => Err(ParserError::ExpectedSymbol {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char().to_string())
                    .collect(),
                found: found.clone(),
            }),
        }
    }

    pub fn peek_parse_close_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Bracket {
                raw,
                depth: d,
                kind: lexer::BracketKind::Close,
            } if brackets.contains(&Bracket::from_close_char(*raw)) && *d == depth => {
                Ok(Bracket::from_close_char(*raw))
            },
            found => Err(ParserError::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found: found.clone(),
            }),
        }
    }

    pub fn parse_open_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Bracket {
                raw,
                depth: d,
                kind: lexer::BracketKind::Open,
            } if brackets.contains(&Bracket::from_open_char(raw)) && d == depth => {
                Ok(Bracket::from_open_char(raw))
            },
            found => Err(ParserError::ExpectedSymbol {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char().to_string())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_close_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Bracket {
                raw,
                depth: d,
                kind: lexer::BracketKind::Close,
            } if brackets.contains(&Bracket::from_close_char(raw)) && d == depth => {
                Ok(Bracket::from_close_char(raw))
            },
            found => Err(ParserError::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found,
            }),
        }
    }
}