use super::*;

#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq, Clone)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Func,
    Let,
}

impl Keyword {
    pub fn match_token(token: &lexer::Token, keywords: &[&str]) -> bool {
        match token {
            lexer::Token::Identifier { raw } => keywords.contains(&raw.as_str()),
            _ => false,
        }
    }

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
    pub fn match_token(token: &lexer::Token) -> bool {
        match token {
            lexer::Token::Identifier { .. } => true,
            _ => false,
        }
    }

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
    pub fn match_token(token: &lexer::Token) -> bool {
        match token {
            lexer::Token::Identifier { .. } => true,
            _ => false,
        }
    }

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
    #[strum(disabled)]
    Unit,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName {
    PrimType(PrimType),
    Struct(String),
    Tuple(Vec<TypeName>),
}

impl TypeName {
    pub fn match_token(token: &lexer::Token) -> bool {
        match token {
            lexer::Token::Bracket { raw: '(', .. } => true,
            lexer::Token::Identifier { .. } => true,
            _ => false,
        }
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.peek_token()? {
            lexer::Token::Bracket { raw: '(', .. } => {
                let tys = Bracketed::parse(lexer, &[Bracket::Round], |lexer| {
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
                        TypeName::parse(lexer)?;
                        token::Separator::Comma;
                        allow_trailing;
                        lexer::Token::Bracket { raw: ')', .. }
                    }
                })?.inner.content;

                match tys.as_slice() {
                    [] => Ok(TypeName::PrimType(PrimType::Unit)),
                    [..] => Ok(TypeName::Tuple(tys)),
                }
            }
            lexer::Token::Identifier { .. } => {
                if let lexer::Token::Identifier { raw } = lexer.next_token().unwrap() {
                    Ok(TypeName::from_str(raw.as_str()).unwrap())
                } else {
                    unreachable!()
                }
            },
            _ => return Err(ParserError::ExpectedIdentifier { found: lexer.next_token().unwrap() }),
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
    pub fn match_token(token: &lexer::Token, ops: &[&str]) -> bool {
        match token {
            lexer::Token::Operator { raw } => ops.contains(&raw.as_str()),
            _ => false,
        }
    }
    
    pub fn peek_parse(lexer: &mut lexer::Lexer, ops: &[&str]) -> Result<Self, ParserError> {
        Operator::parse_token(lexer.peek_token()?, ops)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, ops: &[&str], n: usize) -> Result<Self, ParserError> {
        Operator::parse_token(lexer.peek_nth_token(n)?, ops)
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Operator { raw } => Ok(Operator { op: raw }),
            found => return Err(ParserError::ExpectedOperator { expected: vec!["any".to_owned()], found }),
        }
    }

    pub fn parse_with(lexer: &mut lexer::Lexer, ops: &[&str]) -> Result<Self, ParserError> {
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
    pub fn match_token(token: &lexer::Token, brackets: &[char]) -> bool {
        match token {
            lexer::Token::Bracket { raw, .. } => brackets.contains(raw),
            _ => false,
        }
    }

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
pub enum Separator {
    Comma,
    Semicolon,
}

impl Separator {
    pub fn match_token(token: &lexer::Token, sep: Separator) -> bool {
        match token {
            lexer::Token::Separator { raw } => *raw == sep.into(),
            _ => false,
        }
    }
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
    pub fn match_token(token: &lexer::Token) -> bool {
        match token {
            lexer::Token::StringChar { .. } | lexer::Token::Numeric { .. } => true,
            _ => false,
        }
    }

    pub fn peek_parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        Lit::parse_token(lexer.peek_token()?)
    }

    pub fn peek_nth_parse(lexer: &mut lexer::Lexer, n: usize) -> Result<Self, ParserError> {
        Lit::parse_token(lexer.peek_nth_token(n)?)
    }

    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::StringChar { raw, prefix, suffix, quote: '"' } => {
                Ok(Lit { value: raw, prefix, suffix, kind: LitKind::String })
            },
            lexer::Token::StringChar { raw, prefix, suffix, quote: '\'' } => {
                Ok(Lit { value: raw, prefix, suffix, kind: LitKind::Char })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Integer } => {
                Ok(Lit { value: raw, prefix, suffix, kind: LitKind::Integer })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Float { .. }} => {
                Ok(Lit { value: raw, prefix, suffix, kind: LitKind::Float })
            },
            found => Err(ParserError::ExpectedLiteral { found }),
        }
    }

    pub fn parse_token(token: &lexer::Token) -> Result<Self, ParserError> {
        match token {
            lexer::Token::StringChar { raw, prefix, suffix, quote: '"' } => {
                Ok(Lit {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LitKind::String,
                })
            },
            lexer::Token::StringChar { raw, prefix, suffix, quote: '\'' } => {
                Ok(Lit {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LitKind::Char,
                })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Integer } => {
                Ok(Lit {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LitKind::Integer,
                })
            },
            lexer::Token::Numeric { raw, prefix, suffix, kind: lexer::NumericKind::Float { .. }} => {
                Ok(Lit {
                    value: raw.clone(),
                    prefix: prefix.clone(),
                    suffix: suffix.clone(),
                    kind: LitKind::Float,
                })
            },
            found => Err(ParserError::ExpectedLiteral { found: found.clone() }),
        }
    }
}