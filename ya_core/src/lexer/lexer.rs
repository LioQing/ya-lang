use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Error occurred during IO")]
    IoError(#[from] std::io::Error),

    #[error("Imbalanced brackets found: expected `{expected}`, found `{found}`")]
    ImbalancedBrackets { expected: char, found: char },

    #[error("Invalid numeric format `{raw}`")]
    InvalidNumericFormat { raw: String },

    #[error("Unknown symbol encountered `{symbol}`")]
    UnknownSymbol { symbol: String },
}

#[derive(Debug)]
pub enum Token {
    /** end of the source code */
    Eof,

    /** punctuations including brackets and separators */
    Punctuation { raw: char, kind: PunctuationKind },

    /** numeric literals such as integers, exponent, floats */
    Numeric { raw: String, prefix: String, suffix: String, kind: NumericKind },
}

#[derive(Debug)]
pub enum PunctuationKind {
    Open(usize),
    Close(usize),
    Separator,
}

#[derive(Debug)]
pub enum NumericKind {
    Integer,
    Float { dot_pos: Option<usize>, exp_pos: Option<usize> },
}

pub struct Lexer<'a> {
    pub src: &'a str,
    pub curr: std::iter::Peekable<std::str::Chars<'a>>,

    bracket_stack: std::vec::Vec<char>,
}

impl<'a> Lexer<'a> {
    const OPEN_BRACKETS: &'static [char] = &['(', '{', '[', '<'];
    const CLOSE_BRACKETS: &'static [char] = &[')', '}', ']', '>'];
    const SEPARATORS: &'static [char] = &[',', ';'];

    /** numeric prefix  */
    const NUMERIC_PREFIX: &'static [&'static str] = &["0x", "0o", "0b"];

    pub fn new(src: &'a str) -> Self {
        Lexer {
            src,
            curr: src.chars().peekable(),

            bracket_stack: vec![],
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.ignore_whitespaces();
        self.to_token()
    }

    fn to_token(&mut self) -> Result<Token, LexerError> {
        let c = if let Some(c) = self.curr.next() {
            c
        } else {
            return Ok(Token::Eof);
        };

        match c {
            c if Lexer::OPEN_BRACKETS.contains(&c) => Ok(Token::Punctuation {
                raw: c,
                kind: PunctuationKind::Open(self.push_bracket(c))
            }),
            c if Lexer::CLOSE_BRACKETS.contains(&c) => Ok(Token::Punctuation {
                raw: c,
                kind: PunctuationKind::Close(self.pop_bracket(c)?)
            }),
            c if Lexer::SEPARATORS.contains(&c) => Ok(Token::Punctuation { raw: c, kind: PunctuationKind::Separator }),
            '0'..='9' => self.parse_numeric(c),
            '.' => {
                match self.curr.peek() {
                    Some(&c) if c.is_digit(10) => self.parse_numeric('.'),
                    _ => Err(LexerError::UnknownSymbol { symbol: c.to_string() }),
                }
            },
            c => Err(LexerError::UnknownSymbol { symbol: c.to_string() }),
        }
    }

    fn push_bracket(&mut self, c: char) -> usize {
        self.bracket_stack.push(c);
        self.bracket_stack.len() - 1
    }

    fn pop_bracket(&mut self, c: char) -> Result<usize, LexerError> {
        match self.bracket_stack.last() {
            Some(&last) if Lexer::match_close_bracket(c).unwrap() == last => {
                self.bracket_stack.pop();
                Ok(self.bracket_stack.len())
            },
            Some(&last) => Err(LexerError::ImbalancedBrackets {
                expected: Lexer::match_open_bracket(last).unwrap(),
                found: c,
            }),
            None => Err(LexerError::ImbalancedBrackets {
                expected: ' ',
                found: c,
            }),
        }
    }

    fn parse_numeric(&mut self, first: char) ->Result<Token, LexerError> {
        // TODO: implement radix
        let radix = 10;

        let mut raw = String::new();
        let mut prefix = String::new();
        let mut suffix = String::new();
        let mut kind = NumericKind::Integer;

        // prefix
        for &p in Lexer::NUMERIC_PREFIX {
            if !p.starts_with(first) || p.len() > self.curr.clone().count() {
                continue;
            }

            if p[1..].chars().zip(self.curr.clone().take(p.len() - 1)).all(|(a, b)| a == b) {
                prefix = p.to_owned();
                (0..p.len() - 1).for_each(|_| { self.curr.next(); });
                break;
            }
        }

        if prefix.is_empty() {
            raw.push(first);
            if first == '.' {
                kind = NumericKind::Float { dot_pos: Some(0), exp_pos: None };
            }
        } else {
            match self.curr.peek() {
                Some(&c) if c == '.' => {
                    kind = NumericKind::Float { dot_pos: Some(0), exp_pos: None };
                    raw.push(c);
                },
                Some(&c) if c.is_digit(radix) => raw.push(c),
                _ => return Err(LexerError::InvalidNumericFormat { raw: prefix + &raw }),
            }

            self.curr.next();
        }

        // numeric
        while let Some(&c) = self.curr.peek() {
            match c {
                c if c.is_digit(radix) => {
                    raw.push(c);
                },
                '.' => {
                    if let NumericKind::Integer = kind {
                        kind = NumericKind::Float { dot_pos: Some(raw.len()), exp_pos: None };
                        raw.push(c);
                    } else {
                        return Err(LexerError::InvalidNumericFormat { raw: c.to_string() });
                    }
                },
                'e' | 'E' => {
                    // go to suffix if the char after 'e' or 'E' is not a digit
                    if let Some(c) = self.curr.clone().nth(1) {
                        if !c.is_digit(radix) {
                            break;
                        }
                    }

                    // determine is this exponent or suffix
                    match kind {
                        NumericKind::Float { dot_pos, exp_pos: None } => {
                            kind = NumericKind::Float { dot_pos, exp_pos: Some(raw.len()) };
                        },
                        NumericKind::Integer => {
                            kind = NumericKind::Float { dot_pos: None, exp_pos: Some(raw.len()) };
                        },
                        _ => break,
                    }

                    raw.push(c);
                },
                _ => break,
            }
            
            self.curr.next();
        }

        // suffix
        while let Some(&c) = self.curr.peek() {
            match c {
                c if Lexer::is_identifer_char(c) => {
                    suffix.push(c);
                },
                _ => break,
            }

            self.curr.next();
        }

        Ok(Token::Numeric { raw, prefix, suffix, kind })
    }

    fn match_close_bracket(c: char) -> Option<char> {
        Some(Lexer::OPEN_BRACKETS[Lexer::CLOSE_BRACKETS.iter().position(|&x| x == c)?])
    }

    fn match_open_bracket(c: char) -> Option<char> {
        Some(Lexer::CLOSE_BRACKETS[Lexer::OPEN_BRACKETS.iter().position(|&x| x == c)?])
    }

    fn is_identifer_char(c: char) -> bool {
        c == '_' || (!c.is_ascii_punctuation() && !c.is_ascii_whitespace())
    }

    fn ignore_whitespaces(&mut self) {
        while let Some(&c) = self.curr.peek() {
            if !c.is_ascii_whitespace() {
                break;
            }
            self.curr.next();
        }
    }
}