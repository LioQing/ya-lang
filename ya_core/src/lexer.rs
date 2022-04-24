use thiserror::Error;

#[cfg(test)]
#[path = "lexer_tests.rs"]
mod lexer_tests;

#[derive(Error, Debug, PartialEq)]
pub enum LexerError {
    #[error("Imbalanced brackets found: expected `{expected}`, found `{found}`")]
    ImbalancedBrackets { expected: char, found: char },

    #[error("No valid digits after numeric prefix `{prefix}`")]
    NoValidDigitsAfterNumericPrefix { prefix: String },

    #[error("Unknown symbol encountered `{symbol}`")]
    UnknownSymbol { symbol: String },
}

#[derive(Debug, PartialEq)]
pub enum Token {
    /** end of the source code */
    Eof,

    /** punctuations */
    Punctuation { raw: char, kind: PunctuationKind },

    /** numeric literals */
    Numeric { raw: String, prefix: String, suffix: String, kind: NumericKind },

    /** operators */
    Operator { raw: String },

    /** identifiers */
    Identifier { raw: String },
}

#[derive(Debug, PartialEq)]
pub enum PunctuationKind {
    Open(usize),
    Close(usize),
    Separator,
}

#[derive(Debug, PartialEq)]
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
    const OPEN_BRACKETS: &'static [char] = &['(', '{', '['];
    const CLOSE_BRACKETS: &'static [char] = &[')', '}', ']'];
    const SEPARATORS: &'static [char] = &[',', ';'];

    /** numeric prefix for radix, must start with a digit of radix 10 and have a length greater than 1 */
    const NUMERIC_RADIX_PREFIX: &'static [(&'static str, u32)] = &[("0x", 16), ("0o", 8), ("0b", 2)];

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
            '0'..='9' => self.tokenize_numeric(c),
            '.' => match self.curr.peek() {
                Some(&c) if c.is_digit(10) => self.tokenize_numeric('.'),
                _ => Ok(self.tokenize_operator('.')),
            },
            c if Lexer::is_operator_char(c) => Ok(self.tokenize_operator(c)),
            c if Lexer::is_identifier_char(c) => Ok(self.tokenize_identifier(c)),
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

    fn tokenize_numeric(&mut self, first: char) ->Result<Token, LexerError> {
        let mut raw = String::new();
        let mut prefix = String::new();
        let mut suffix = String::new();
        let mut kind = NumericKind::Integer;

        let mut radix = 10;

        // prefix
        for (p, r) in Lexer::NUMERIC_RADIX_PREFIX {
            if !p.starts_with(first) || p.len() - 1 > self.curr.clone().count() {
                continue;
            }

            if p[1..].chars().zip(self.curr.clone().take(p.len() - 1)).all(|(a, b)| a == b) {
                prefix = p.clone().to_owned();
                (0..p.len() - 1).for_each(|_| { self.curr.next(); });
                radix = *r;
                break;
            }
        }

        // numeric
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
                _ => return Err(LexerError::NoValidDigitsAfterNumericPrefix { prefix }),
            }

            self.curr.next();
        }

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
                        return Ok(Token::Numeric { raw, prefix, suffix, kind });
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
                c if Lexer::is_identifier_char(c) => {
                    suffix.push(c);
                },
                _ => break,
            }

            self.curr.next();
        }

        Ok(Token::Numeric { raw, prefix, suffix, kind })
    }

    fn tokenize_operator(&mut self, first: char) -> Token {
        let mut raw = first.to_string();

        while let Some(&c) = self.curr.peek() {
            if !Lexer::is_operator_char(c) {
                break;
            }

            raw.push(c);
            self.curr.next();
        }

        Token::Operator { raw }
    }

    fn tokenize_identifier(&mut self, first: char) -> Token {
        let mut raw = first.to_string();

        while let Some(&c) = self.curr.peek() {
            if !Lexer::is_identifier_char(c) {
                break;
            }
            
            raw.push(c);
            self.curr.next();
        }

        Token::Identifier { raw }
    }

    fn match_close_bracket(c: char) -> Option<char> {
        Some(Lexer::OPEN_BRACKETS[Lexer::CLOSE_BRACKETS.iter().position(|&x| x == c)?])
    }

    fn match_open_bracket(c: char) -> Option<char> {
        Some(Lexer::CLOSE_BRACKETS[Lexer::OPEN_BRACKETS.iter().position(|&x| x == c)?])
    }

    fn is_identifier_char(c: char) -> bool {
        c == '_' || (!c.is_ascii_punctuation() && !c.is_ascii_whitespace())
    }

    fn is_operator_char(c: char) -> bool {
        c != '_' && !Lexer::OPEN_BRACKETS.contains(&c) && !Lexer::CLOSE_BRACKETS.contains(&c) &&
        !Lexer::SEPARATORS.contains(&c) && c.is_ascii_punctuation()
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