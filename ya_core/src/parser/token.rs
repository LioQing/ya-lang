use super::*;

#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Func,
    Let,
}

impl Keyword {
    pub fn parse(lexer: &mut lexer::Lexer, keywords: &[&str]) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } if keywords.contains(&raw.as_str()) => Ok(Keyword::from_str(&raw)?),
            found => Err(ParserError::ExpectedKeyword {
                expected: keywords.iter().map(|s| s.to_string()).collect(),
                found
            }),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncName {
    pub name: String,
}

impl FuncName {
    pub fn parse(lexer: &mut lexer::Lexer) -> Result<Self, ParserError> {
        match lexer.next_token()? {
            lexer::Token::Identifier { raw } => Ok(FuncName { name: raw }),
            found => return Err(ParserError::ExpectedIdentifier { found }),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct VarName {
    pub name: String,
}

impl VarName {
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

#[derive(Debug, PartialEq)]
pub enum TypeName {
    PrimType(PrimType),
    Struct(String),
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
}

#[derive(Debug, PartialEq)]
pub struct Bracketed<T> {
    pub inner: T,
    pub bracket: Bracket,
}

impl<T> Bracketed<T> {
    pub fn parse<F>(lexer: &mut lexer::Lexer, brackets: &[Bracket], f: F) -> Result<Self, ParserError>
    where
        F: Fn(&mut lexer::Lexer) -> Result<T, ParserError>
    {
        let bracket = match lexer.next_token()? {
            lexer::Token::Bracket {
                raw,
                kind: lexer::BracketKind::Open,
                ..
            } if brackets.iter().any(|b| b.to_open_char() == raw) => Bracket::from_open_char(raw),
            found => return Err(ParserError::ExpectedSymbol {
                expected: brackets.iter().map(|b| b.to_open_char().to_string()).collect(),
                found
            }),
        };

        let inner = f(lexer)?;

        match lexer.next_token()? {
            lexer::Token::Bracket {
                raw,
                kind: lexer::BracketKind::Close,
                ..
            } if bracket.to_close_char() == raw => {
                Ok(Bracketed {
                    inner,
                    bracket,
                })
            },
            found => Err(ParserError::ExpectedBracket {
                expected: vec![bracket.to_close_char().to_string()],
                found
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
    pub content: Vec<T>,
    pub separator: Separator,
}

impl<T> Separated<T> {
    pub fn parse<F>(lexer: &mut lexer::Lexer, sep: Separator, parse: F) -> Result<Self, ParserError>
    where
        F: Fn(&mut lexer::Lexer) -> (Result<T, ParserError>, bool)
    {
        let mut content = Vec::new();

        loop {
            let (res, cont) = parse(lexer);
            content.push(res?);

            if !cont {
                break;
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