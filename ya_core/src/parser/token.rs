use super::*;

#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq, Clone)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Func,
    Let,
}

impl Keyword {
    pub fn peek_parse(lexer: &mut lexer::Lexer, keywords: &[&str]) -> Result<Self, ParserError> {
        Keyword::parse_token(lexer.peek_token()?, keywords)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, keywords: &[&str], n: usize) -> Result<Self, ParserError> {
        Keyword::parse_token(lexer.peek_nth_token(n)?, keywords)
    }

    pub fn parse(lexer: &mut lexer::Lexer, keywords: &[&str]) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } if keywords.contains(&raw.as_str()) => Ok(Keyword::from_str(raw.as_str())?),
            found => Err(ParserError::ExpectedKeyword {
                expected: keywords.iter().map(|s| s.to_string()).collect(),
                found,
            }),
        }
    }

    pub fn parse_token(token: &lexer::Token, keywords: &[&str]) -> Result<Self, ParserError> {
        match token {
            lexer::Token::Identifier { raw } if keywords.contains(&raw.as_str()) => Ok(Keyword::from_str(raw.as_str())?),
            found => Err(ParserError::ExpectedKeyword {
                expected: keywords.iter().map(|s| s.to_string()).collect(),
                found: found.clone(),
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
        FuncName::parse_token(lexer.peek_token()?)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, n: usize) -> Result<Self, ParserError> {
        FuncName::parse_token(lexer.peek_nth_token(n)?)
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } => Ok(FuncName { name: raw }),
            found => return Err(ParserError::ExpectedIdentifier { found }),
        }
    }

    pub fn parse_token(token: &lexer::Token) -> Result<Self, ParserError> {
        match token {
            lexer::Token::Identifier { raw } => Ok(FuncName { name: raw.clone() }),
            found => return Err(ParserError::ExpectedIdentifier { found: found.clone() }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarName {
    pub name: String,
}

impl VarName {
    pub fn peek_parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        VarName::parse_token(lexer.peek_token()?)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, n: usize) -> Result<Self, ParserError> {
        VarName::parse_token(lexer.peek_nth_token(n)?)
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } => Ok(VarName { name: raw }),
            found => return Err(ParserError::ExpectedIdentifier { found }),
        }
    }

    pub fn parse_token(token: &lexer::Token) -> Result<Self, ParserError> {
        match token {
            lexer::Token::Identifier { raw } => Ok(VarName { name: raw.clone() }),
            found => return Err(ParserError::ExpectedIdentifier { found: found.clone() }),
        }
    }
}

#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum PrimType {
    Unit, I8, I16, I32, I64,
    U8, U16, U32, U64, F32, F64, Bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName {
    PrimType(PrimType),
    Struct(String),
}

impl TypeName {
    pub fn peek_parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        TypeName::parse_token(lexer.peek_token()?)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, n: usize) -> Result<Self, ParserError> {
        TypeName::parse_token(lexer.peek_nth_token(n)?)
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } => Ok(TypeName::from_str(&raw).unwrap()),
            found => return Err(ParserError::ExpectedIdentifier { found }),
        }
    }

    pub fn parse_token(token: &lexer::Token) -> Result<Self, ParserError> {
        match token {
            lexer::Token::Identifier { raw } => Ok(TypeName::from_str(&raw).unwrap()),
            found => return Err(ParserError::ExpectedIdentifier { found: found.clone() }),
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
        Operator::parse_token(lexer.peek_token()?, ops)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, ops: &[&str], n: usize) -> Result<Self, ParserError> {
        Operator::parse_token(lexer.peek_nth_token(n)?, ops)
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

    pub fn parse_token(token: &lexer::Token, ops: &[&str]) -> Result<Self, ParserError> {
        match token {
            lexer::Token::Operator { raw } if ops.contains(&raw.as_str()) => Ok(Operator { op: raw.clone() }),
            found => Err(ParserError::ExpectedOperator {
                expected: ops.iter().map(|s| s.to_string()).collect(),
                found: found.clone(),
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
        Bracket::parse_token_open(lexer.peek_token()?, brackets)
    }

    pub fn peek_nth_parse_open(lexer: &mut lexer::Lexer, brackets: &[Bracket], n: usize) -> Result<Self, ParserError> {
        Bracket::parse_token_open(lexer.peek_nth_token(n)?, brackets)
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
            found => Err(ParserError::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_token_open(token: &lexer::Token, brackets: &[Bracket]) -> Result<Self, ParserError> {
        match token {
            lexer::Token::Bracket {
                raw,
                kind: lexer::BracketKind::Open,
                ..
            } if brackets.contains(&Bracket::from_open_char(*raw)) => {
                Ok(Bracket::from_open_char(*raw))
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

    pub fn peek_parse_close(lexer: &mut lexer::Lexer, brackets: &[Bracket]) -> Result<Self, ParserError> {
        Bracket::parse_token_close(lexer.peek_token()?, brackets)
    }

    pub fn peek_nth_parse_close(lexer: &mut lexer::Lexer, brackets: &[Bracket], n: usize) -> Result<Self, ParserError> {
        Bracket::parse_token_close(lexer.peek_nth_token(n)?, brackets)
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
                    .map(|b| b.to_close_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_token_close(token: &lexer::Token, brackets: &[Bracket]) -> Result<Self, ParserError> {
        match token {
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
                    .map(|b| b.to_close_char())
                    .collect(),
                found: found.clone(),
            }),
        }
    }

    pub fn peek_parse_open_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        Bracket::parse_token_open_with_depth(lexer.peek_token()?, brackets, depth)
    }

    pub fn peek_nth_parse_open_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        n: usize,
        depth: usize
    ) -> Result<Self, ParserError> {
        Bracket::parse_token_open_with_depth(lexer.peek_nth_token(n)?, brackets, depth)
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
            found => Err(ParserError::ExpectedBracket {
                expected: brackets
                    .iter()
                    .map(|b| b.to_open_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_token_open_with_depth(
        token: &lexer::Token,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        match token {
            lexer::Token::Bracket {
                raw,
                depth: d,
                kind: lexer::BracketKind::Open,
            } if brackets.contains(&Bracket::from_open_char(*raw)) && *d == depth => {
                Ok(Bracket::from_open_char(*raw))
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

    pub fn peek_parse_close_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        Bracket::parse_token_close_with_depth(lexer.peek_token()?, brackets, depth)
    }

    pub fn peek_nth_parse_close_with_depth(
        lexer: &mut lexer::Lexer,
        brackets: &[Bracket],
        n: usize,
        depth: usize
    ) -> Result<Self, ParserError> {
        Bracket::parse_token_close_with_depth(lexer.peek_nth_token(n)?, brackets, depth)
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
                    .map(|b| b.to_close_char())
                    .collect(),
                found,
            }),
        }
    }

    pub fn parse_token_close_with_depth(
        token: &lexer::Token,
        brackets: &[Bracket],
        depth: usize
    ) -> Result<Self, ParserError> {
        match token {
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
                    .map(|b| b.to_close_char())
                    .collect(),
                found: found.clone(),
            }),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LiteralKind {
    String,
    Char,
    Integer,
    Float,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub value: String,
    pub prefix: String,
    pub suffix: String,
    pub kind: LiteralKind,
}

impl Literal {
    pub fn peek_parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        Literal::parse_token(lexer.peek_token()?)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, n: usize) -> Result<Self, ParserError> {
        Literal::parse_token(lexer.peek_nth_token(n)?)
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::StringChar { raw, prefix, suffix, quote: '"' } => {
                Ok(Literal { value: raw, prefix, suffix, kind: LiteralKind::String })
            },
            lexer::Token::StringChar { raw, prefix, suffix, quote: '\'' } => {
                Ok(Literal { value: raw, prefix, suffix, kind: LiteralKind::Char })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Integer } => {
                Ok(Literal { value: raw, prefix, suffix, kind: LiteralKind::Integer })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Float { .. }} => {
                Ok(Literal { value: raw, prefix, suffix, kind: LiteralKind::Float })
            },
            found => Err(ParserError::ExpectedLiteral { found }),
        }
    }

    pub fn parse_token(token: &lexer::Token) -> Result<Self, ParserError> {
        match token {
            lexer::Token::StringChar { raw, prefix, suffix, quote: '"' } => {
                Ok(Literal {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LiteralKind::String,
                })
            },
            lexer::Token::StringChar { raw, prefix, suffix, quote: '\'' } => {
                Ok(Literal {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LiteralKind::Char,
                })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Integer } => {
                Ok(Literal {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LiteralKind::Integer,
                })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Float { .. }} => {
                Ok(Literal {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LiteralKind::Float,
                })
            },
            found => Err(ParserError::ExpectedLiteral { found: found.clone() }),
        }
    }
}