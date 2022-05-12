use std::collections::VecDeque;
use thiserror::Error;
use std::ops::Range;

#[cfg(test)]
mod tests;

#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum Error {
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Span {
    pub line: usize,
    pub col_range: Range<usize>,
    pub codepoint_range: Range<usize>,
    pub dist_from_prev: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenKind {
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

impl TokenKind {
    pub fn is_punc(&self) -> bool {
        match self {
            TokenKind::Separator { .. } | TokenKind::Operator { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BracketKind {
    Open,
    Close,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum NumericKind {
    Integer,
    Float { dot_pos: Option<usize>, exp_pos: Option<usize> },
}

#[derive(Debug, Clone)]
struct CodeIter<'a> {
    peekable: std::iter::Peekable<std::str::Chars<'a>>,
    counter: usize,
}

impl<'a> CodeIter<'a> {
    fn new(code: &'a str) -> Self {
        Self {
            peekable: code.chars().peekable(),
            counter: 0,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.peekable.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.counter += 1;
        self.peekable.next()
    }

    fn count(self) -> usize {
        self.peekable.count()
    }

    fn take(self, n: usize) -> std::iter::Take<std::iter::Peekable<std::str::Chars<'a>>> {
        self.peekable.take(n)
    }

    fn nth(&mut self, n: usize) -> Option<char> {
        self.peekable.nth(n)
    }
}

/// The lexer.
/// 
/// Performs lexical analysis.
/// Stores the source code.
/// Provides methods to get and peek the next token.
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    curr: CodeIter<'a>,
    curr_line: usize,
    curr_col: usize,
    curr_codepoint: usize,

    bracket_stack: Vec<char>,
    buf: VecDeque<Result<Token, Error>>,
}

impl<'a> Lexer<'a> {
    const OPEN_BRACKETS: &'static [char] = &['(', '{', '['];
    const CLOSE_BRACKETS: &'static [char] = &[')', '}', ']'];
    const SEPARATORS: &'static [char] = &[',', ';'];

    /** numeric prefix for radix, must start with a digit of radix 10 and have a length greater than 1 */
    const NUMERIC_RADIX_PREFIX: &'static [(&'static str, u32)] = &[("0x", 16), ("0o", 8), ("0b", 2)];

    pub fn new(src: &'a str) -> Self {
        Self {
            curr: CodeIter::new(src),
            curr_line: 1,
            curr_col: 0,
            curr_codepoint: 0,

            bracket_stack: vec![],
            buf: VecDeque::new(),
        }
    }

    pub fn skip(&mut self, n: usize) {
        (0..n).for_each(|_| { self.next_token().ok(); });
    }

    pub fn peek_token(&mut self) -> Result<&Token, Error> {
        if self.buf.is_empty() {
            let res = self.consume_next_token();
            self.buf.push_back(res);
        }

        match self.buf.front().unwrap() {
            Ok(t) => Ok(t),
            Err(e) => Err(e.clone()),
        }
    }

    pub fn peek_nth_token(&mut self, n: usize) -> Result<&Token, Error> {
        for _ in self.buf.len()..=n {
            let res = self.consume_next_token();
            self.buf.push_back(res);
        }

        match self.buf.get(n).unwrap() {
            Ok(t) => Ok(t),
            Err(e) => Err(e.clone()),
        }
    }

    pub fn peek_range_token(&mut self, r: std::ops::Range<usize>) -> Vec<Result<&Token, Error>> {
        for _ in self.buf.len()..=r.end {
            let res = self.consume_next_token();
            self.buf.push_back(res);
        }

        self.buf
            .iter()
            .skip(r.start)
            .map(|t| t
                .as_ref()
                .map_err(|e| e.clone()))
            .take(r.len())
            .collect()
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        if let Some(token) = self.buf.pop_front() {
            token
        } else {
            self.consume_next_token()
        }
    }

    pub fn is_next_whitespace(&mut self) -> bool {
        self.curr.peek().map_or(false, |c| c.is_ascii_whitespace())
    }

    fn consume_next_token(&mut self) -> Result<Token, Error> {
        let dist_from_prev = self.ignore_whitespaces();
        self.curr.counter = 0;

        let c = if let Some(c) = self.curr.next() {
            c
        } else {
            self.curr_col += 1;
            self.curr_codepoint += 1;

            return Ok(Token {
                kind: TokenKind::Eof,
                span: Span {
                    line: self.curr_line,
                    col_range: self.curr_col - 1..self.curr_col,
                    codepoint_range: self.curr_codepoint - 1..self.curr_codepoint,
                    dist_from_prev,
                },
            });
        };

        let kind = match c {
            c if Self::OPEN_BRACKETS.contains(&c) => Ok(self.tokenize_open_bracket(c)),
            c if Self::CLOSE_BRACKETS.contains(&c) => self.tokenize_close_bracket(c),
            c if Self::SEPARATORS.contains(&c) => Ok(TokenKind::Separator { raw: c }),
            '0'..='9' => self.tokenize_numeric(c),
            '.' => match self.curr.peek() {
                Some(&c) if c.is_digit(10) => self.tokenize_numeric('.'),
                _ => Ok(self.tokenize_operator('.')),
            },
            c @ ('"' | '\'') => self.tokenize_string_char(c, "".to_owned()),
            c if Self::is_operator_char(c) => Ok(self.tokenize_operator(c)),
            c if Self::is_identifier_char(c) => self.tokenize_identifier(c),
            c => Err(Error::UnknownSymbol { symbol: c.to_string() }),
        };

        self.curr_col += self.curr.counter;
        self.curr_codepoint += self.curr.counter;

        Ok(Token {
            kind: kind?,
            span: Span {
                line: self.curr_line,
                col_range: self.curr_col - self.curr.counter..self.curr_col,
                codepoint_range: self.curr_codepoint - self.curr.counter..self.curr_codepoint,
                dist_from_prev
            },
        })
    }

    fn tokenize_open_bracket(&mut self, c: char) -> TokenKind {
        self.bracket_stack.push(c);
        TokenKind::Bracket { raw: c, depth: self.bracket_stack.len() - 1, kind: BracketKind::Open }
    }

    fn tokenize_close_bracket(&mut self, c: char) -> Result<TokenKind, Error> {
        match self.bracket_stack.last() {
            Some(&last) if Self::match_close_bracket(c).unwrap() == last => {
                self.bracket_stack.pop();
                Ok(TokenKind::Bracket { raw: c, depth: self.bracket_stack.len(), kind: BracketKind::Close })
            },
            Some(&last) => Err(Error::MismatchedBrackets {
                expected: Self::match_open_bracket(last).unwrap(),
                found: c,
            }),
            None => Err(Error::NoOpeningBracket { close: c }),
        }
    }

    fn tokenize_numeric(&mut self, first: char) ->Result<TokenKind, Error> {
        let mut raw = String::new();
        let mut prefix = String::new();
        let mut suffix = String::new();
        let mut kind = NumericKind::Integer;

        let mut radix = 10;

        // prefix
        for (p, r) in Self::NUMERIC_RADIX_PREFIX {
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
                _ => return Err(Error::NoDigitsAfterNumericPrefix { prefix }),
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
                        return Ok(TokenKind::Numeric { raw, prefix, suffix, kind });
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
            if !Self::is_identifier_char(c) {
                break;
            }

            suffix.push(c);
            self.curr.next();
        }

        Ok(TokenKind::Numeric { raw, prefix, suffix, kind })
    }
    
    fn tokenize_string_char(&mut self, quote: char, prefix: String) -> Result<TokenKind, Error> {
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
                            return Err(Error::NoClosingQuote { raw: prefix + &quote.to_string() + &raw + "\\" })
                        },
                    }
                },
                _ => raw.push(c),
            }

            self.curr.next();

            if self.curr.peek().is_none() {
                return Err(Error::NoClosingQuote { raw: prefix + &quote.to_string() + &raw })
            }
        }

        // suffix
        while let Some(&c) = self.curr.peek() {
            if !Self::is_identifier_char(c) {
                break;
            }

            suffix.push(c);
            self.curr.next();
        }

        if let Some(sequence) = err {
            Err(Error::UnknownEscapeSequence {
                raw: prefix + &quote.to_string() + &raw + &quote.to_string() + &suffix,
                sequence
            })
        } else {
            Ok(TokenKind::StringChar { raw, prefix, suffix, quote })
        }
    }

    fn tokenize_operator(&mut self, first: char) -> TokenKind {
        let mut raw = first.to_string();

        while let Some(&c) = self.curr.peek() {
            if !Self::is_operator_char(c) {
                break;
            }

            raw.push(c);
            self.curr.next();
        }

        TokenKind::Operator { raw }
    }

    fn tokenize_identifier(&mut self, first: char) -> Result<TokenKind, Error> {
        let mut raw = first.to_string();

        while let Some(&c) = self.curr.peek() {
            if c == '"' || c == '\'' {
                self.curr.next();
                return self.tokenize_string_char(c, raw);
            } else if !Self::is_identifier_char(c) {
                break;
            }
            
            raw.push(c);
            self.curr.next();
        }

        Ok(TokenKind::Identifier { raw })
    }

    fn ignore_whitespaces(&mut self) -> usize {
        let mut count = 0;
        while self.is_next_whitespace() {
            if matches!(self.curr.next(), Some(c) if c == '\n') {
                self.curr_line += 1;
                self.curr_col = 0;
            } else {
                self.curr_col += 1;
            }
            count += 1;
        }
        self.curr_codepoint += count;
        count
    }

    fn match_close_bracket(c: char) -> Option<char> {
        Some(Self::OPEN_BRACKETS[Self::CLOSE_BRACKETS.iter().position(|&x| x == c)?])
    }

    fn match_open_bracket(c: char) -> Option<char> {
        Some(Self::CLOSE_BRACKETS[Self::OPEN_BRACKETS.iter().position(|&x| x == c)?])
    }

    fn is_identifier_char(c: char) -> bool {
        c == '_' || (!c.is_ascii_punctuation() && !c.is_ascii_whitespace())
    }

    fn is_operator_char(c: char) -> bool {
        c != '_' && !Self::OPEN_BRACKETS.contains(&c) && !Self::CLOSE_BRACKETS.contains(&c) &&
        !Self::SEPARATORS.contains(&c) && c.is_ascii_punctuation()
    }
}