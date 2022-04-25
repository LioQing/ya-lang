use thiserror::Error;

#[cfg(test)]
#[path = "lexer_tests.rs"]
mod lexer_tests;

#[derive(Error, Debug, PartialEq)]
pub enum LexerError {
    #[error("No open bracket found for closing bracket `{close}`")]
    NoOpeningBracket { close: char },

    #[error("Mismatched brackets found: expected `{expected}`, found `{found}`")]
    MismatchedBrackets { expected: char, found: char },

    #[error("No digits found after numeric prefix `{prefix}`")]
    NoDigitsAfterNumericPrefix { prefix: String },

    #[error("Unknown escape sequence `{sequence}` found in `{raw}`")]
    UnknownEscapeSequence { raw: String, sequence: String },

    #[error("No closing quotattion mark found for `{raw}`")]
    NoClosingQuote { raw: String },

    #[error("Unknown symbol found `{symbol}`")]
    UnknownSymbol { symbol: String },
}

#[derive(Debug, PartialEq)]
pub enum Token {
    /** end of the source code */
    Eof,

    /** brackets */
    Bracket { raw: char, depth: usize, kind: BracketKind },

    /** separators */
    Separator { raw: char },

    /** numeric literals */
    Numeric { raw: String, prefix: String, suffix: String, kind: NumericKind },

    /** strings or char literals */
    StringChar { raw: String, prefix: String, suffix: String, quote: char },

    /** operators */
    Operator { raw: String },

    /** identifiers */
    Identifier { raw: String },
}

#[derive(Debug, PartialEq)]
pub enum BracketKind {
    Open,
    Close,
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
            c if Lexer::OPEN_BRACKETS.contains(&c) => Ok(self.tokenize_open_bracket(c)),
            c if Lexer::CLOSE_BRACKETS.contains(&c) => self.tokenize_close_bracket(c),
            c if Lexer::SEPARATORS.contains(&c) => Ok(Token::Separator { raw: c }),
            '0'..='9' => self.tokenize_numeric(c),
            '.' => match self.curr.peek() {
                Some(&c) if c.is_digit(10) => self.tokenize_numeric('.'),
                _ => Ok(self.tokenize_operator('.')),
            },
            c @ ('"' | '\'') => self.tokenize_string_char(c, "".to_owned()),
            c if Lexer::is_operator_char(c) => Ok(self.tokenize_operator(c)),
            c if Lexer::is_identifier_char(c) => self.tokenize_identifier(c),
            c => Err(LexerError::UnknownSymbol { symbol: c.to_string() }),
        }
    }

    fn tokenize_open_bracket(&mut self, c: char) -> Token {
        self.bracket_stack.push(c);
        Token::Bracket { raw: c, depth: self.bracket_stack.len() - 1, kind: BracketKind::Open }
    }

    fn tokenize_close_bracket(&mut self, c: char) -> Result<Token, LexerError> {
        match self.bracket_stack.last() {
            Some(&last) if Lexer::match_close_bracket(c).unwrap() == last => {
                self.bracket_stack.pop();
                Ok(Token::Bracket { raw: c, depth: self.bracket_stack.len(), kind: BracketKind::Close })
            },
            Some(&last) => Err(LexerError::MismatchedBrackets {
                expected: Lexer::match_open_bracket(last).unwrap(),
                found: c,
            }),
            None => Err(LexerError::NoOpeningBracket { close: c }),
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
                _ => return Err(LexerError::NoDigitsAfterNumericPrefix { prefix }),
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
                    match self.curr.clone().nth(1) {
                        Some(c) if !c.is_digit(radix) => break,
                        None => break,
                        _ => {},
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
            if !Lexer::is_identifier_char(c) {
                break;
            }

            suffix.push(c);
            self.curr.next();
        }

        Ok(Token::Numeric { raw, prefix, suffix, kind })
    }
    
    fn tokenize_string_char(&mut self, quote: char, prefix: String) -> Result<Token, LexerError> {
        let mut raw = String::new();
        let mut suffix = String::new();

        let mut err = None;

        // string
        while let Some(&c) = self.curr.peek() {
            match c {
                '"' | '\'' => {
                    if c == quote {
                        self.curr.next();
                        break;
                    } else {
                        raw.push(c);
                    }
                },
                '\\' => {
                    self.curr.next();
                    match self.curr.peek() {
                        Some('\'') => raw.push('\''),
                        Some('"') => raw.push('"'),
                        Some('\\') => raw.push('\\'),
                        Some('n') => raw.push('\n'),
                        Some('r') => raw.push('\r'),
                        Some('t') => raw.push('\t'),
                        Some('0') => raw.push('\0'),
                        Some(&c) => {
                            if err.is_none() {
                                err = Some("\\".to_owned() + &c.to_string());
                            }

                            raw.push('\\');
                            raw.push(c);
                        },
                        None => {
                            self.curr.next();
                            return Err(LexerError::NoClosingQuote { raw: prefix + &quote.to_string() + &raw + "\\" })
                        },
                    }
                },
                _ => raw.push(c),
            }

            self.curr.next();

            if self.curr.peek().is_none() {
                return Err(LexerError::NoClosingQuote { raw: prefix + &quote.to_string() + &raw })
            }
        }

        // suffix
        while let Some(&c) = self.curr.peek() {
            if !Lexer::is_identifier_char(c) {
                break;
            }

            suffix.push(c);
            self.curr.next();
        }

        if let Some(sequence) = err {
            Err(LexerError::UnknownEscapeSequence {
                raw: prefix + &quote.to_string() + &raw + &quote.to_string() + &suffix,
                sequence
            })
        } else {
            Ok(Token::StringChar { raw, prefix, suffix, quote })
        }
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

    fn tokenize_identifier(&mut self, first: char) -> Result<Token, LexerError> {
        let mut raw = first.to_string();

        while let Some(&c) = self.curr.peek() {
            if c == '"' || c == '\'' {
                self.curr.next();
                return self.tokenize_string_char(c, raw);
            } else if !Lexer::is_identifier_char(c) {
                break;
            }
            
            raw.push(c);
            self.curr.next();
        }

        Ok(Token::Identifier { raw })
    }

    fn ignore_whitespaces(&mut self) {
        while let Some(&c) = self.curr.peek() {
            if !c.is_ascii_whitespace() {
                break;
            }
            self.curr.next();
        }
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
}