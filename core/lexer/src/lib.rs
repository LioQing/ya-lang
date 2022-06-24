use token::*;

pub mod error;
use error::*;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct CodeIter<'a> {
    curr: std::iter::Peekable<std::str::Chars<'a>>,
    count: usize,
}

impl<'a> CodeIter<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            curr: code.chars().peekable(),
            count: 0,
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.curr.peek()
    }

    // reteurn and then reset count
    pub fn get_count(&mut self) -> usize {
        let count = self.count;
        self.count = 0;
        count
    }
}

impl<'a> std::iter::Iterator for CodeIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;
        self.curr.next()
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    curr: CodeIter<'a>,
    line: usize,
    col: usize,
    codepoint: usize,

    paren_stack: Vec<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            curr: CodeIter::new(src),
            line: 0,
            col: 0,
            codepoint: 0,

            paren_stack: vec![],
        }
    }

    fn tokenize_open_paren(&mut self, first: char) -> Result<Token, Error> {
        self.paren_stack.push(first);
        
        Ok(Token::new_kind(TokenKind::Paren {
            raw: first,
            depth: self.paren_stack.len() - 1,
            kind: ParenKind::Open,
        }))
    }

    fn tokenize_close_paren(&mut self, first: char) -> Result<Token, Error> {        
        match self.paren_stack.last() {
            Some(&open) if Self::are_parens_match(open, first) => {
                self.paren_stack.pop();
                Ok(Token::new_kind(TokenKind::Paren {
                    raw: first,
                    depth: self.paren_stack.len(),
                    kind: ParenKind::Close,
                }))
            },
            Some(&open) => Err(Error::new_kind(
                ErrorKind::MismatchedParens(
                    Self::get_matching_paren(open).unwrap(),
                    first,
                ),
            )),
            None => Err(Error::new_kind(
                ErrorKind::MissingOpenParen(first),
            )),
        }
    }

    fn tokenize_num(&mut self, first: char) -> Result<Token, Error> {
        /** numeric prefix for radix, must start with a digit of radix 10 and have a length greater than 1 */
        const NUMERIC_RADIX_PREFIX: &'static [(&'static str, u32)] = &[("0x", 16), ("0o", 8), ("0b", 2)];

        let mut raw = String::new();
        let mut prefix = String::new();
        let mut suffix = String::new();
        let mut kind = LitKind::Integer;

        let mut radix = 10;

        // prefix
        for (p, r) in NUMERIC_RADIX_PREFIX {
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

        let prefix = prefix;

        // raw
        if prefix.is_empty() {
            raw.push(first);
            if first == '.' {
                kind = LitKind::Float { dot_pos: Some(0), exp_pos: None };
            }
        } else {
            match self.curr.peek() {
                Some(&c) if c == '.' => {
                    kind = LitKind::Float { dot_pos: Some(0), exp_pos: None };
                    raw.push(c);
                },
                Some(&c) if c.is_digit(radix) => raw.push(c),
                _ => return Err(Error::new_kind(
                    ErrorKind::MissingDigitAfterPrefix(prefix.clone()),
                )),
            }

            self.curr.next();
        }

        while let Some(&c) = self.curr.peek() {
            match c {
                c if c.is_digit(radix) => {
                    raw.push(c);
                },
                '.' => {
                    if let LitKind::Integer = kind {
                        kind = LitKind::Float { dot_pos: Some(raw.len()), exp_pos: None };
                        raw.push(c);
                    } else {
                        return Ok(Token::new_kind(
                            TokenKind::Lit { raw, prefix, suffix, kind },
                        ));
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
                        LitKind::Float { dot_pos, exp_pos: None } => {
                            kind = LitKind::Float { dot_pos, exp_pos: Some(raw.len()) };
                        },
                        LitKind::Integer => {
                            kind = LitKind::Float { dot_pos: None, exp_pos: Some(raw.len()) };
                        },
                        _ => break,
                    }

                    raw.push(c);
                },
                _ => break,
            }

            self.curr.next();
        }

        let raw = raw;

        // suffix
        while let Some(&c) = self.curr.peek() {
            if c.is_ascii_punctuation() || c.is_whitespace() {
                break;
            }

            suffix.push(c);
            self.curr.next();
        }

        Ok(Token::new_kind(
            TokenKind::Lit { raw, prefix, suffix, kind },
        ))
    }

    fn tokenize_punc(&mut self, first: char) -> Result<Token, Error> {
        let mut raw = first.to_string();
        
        while let Some(&c) = self.curr.peek() {
            if !c.is_ascii_punctuation()
                || c == '_'
                || ['(', '{', '[', ')', '}', ']'].contains(&c)
            {
                break;
            }
            
            raw.push(c);
            self.curr.next();
        }

        Ok(Token::new_kind(
            TokenKind::Punc { raw },
        ))
    }

    fn tokenize_quote(&mut self, first: char, prefix: String) -> Result<Token, Error> {        
        let mut raw = String::new();
        let mut suffix = String::new();

        let mut esc_seq_err = None;

        // string
        while let Some(&c) = self.curr.peek() {
            match c {
                '"' | '\'' if c == first => {
                    self.curr.next();
                    break;
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
                            match &mut esc_seq_err {
                                None => esc_seq_err = Some(vec!["\\".to_owned() + &c.to_string()]),
                                Some(v) => v.push("\\".to_owned() + &c.to_string()),
                            }

                            raw.push('\\');
                            raw.push(c);
                        },
                        None => {
                            self.curr.next();
                            return Err(Error::new_kind(
                                ErrorKind::MissingCloseQuote(raw),
                            ));
                        },
                    }
                },
                _ => raw.push(c),
            }

            self.curr.next();

            if self.curr.peek().is_none() {
                self.curr.next();
                return Err(Error::new_kind(
                    ErrorKind::MissingCloseQuote(raw),
                ))
            }
        }

        let raw = raw;

        // suffix
        while let Some(&c) = self.curr.peek() {
            if c.is_ascii_punctuation() || c.is_whitespace() {
                break;
            }

            suffix.push(c);
            self.curr.next();
        }

        if let Some(seqs) = esc_seq_err {
            Err(Error::new_kind(
                ErrorKind::InvalidEscapeSequence(seqs),
            ))
        } else {
            Ok(Token::new_kind(
                TokenKind::Lit { raw, prefix, suffix, kind: LitKind::Quote { quote: first } },
            ))
        }
    }

    fn tokenize_id(&mut self, first: char) -> Result<Token, Error> {
        let mut raw = first.to_string();
        
        while let Some(&c) = self.curr.peek() {
            if c == '"' || c == '\'' {
                self.curr.next();
                return self.tokenize_quote(c, raw);
            } else if c != '_' && (c.is_ascii_punctuation() || c.is_whitespace()) {
                break;
            }
            
            raw.push(c);
            self.curr.next();
        }

        Ok(Token::new_kind(
            TokenKind::Id { raw },
        ))
    }

    fn is_next_whitespace(&mut self) -> bool {
        self.curr.peek().map_or(false, |c| c.is_ascii_whitespace())
    }

    fn ignore_whitespaces(&mut self) -> usize {
        while self.is_next_whitespace() {
            match self.curr.next() {
                Some(c) if c == '\n' => {
                    self.line += 1;
                    self.col = 0;
                },
                _ => {
                    self.col += 1;
                },
            }
        }
        let count = self.curr.get_count();
        self.codepoint += count;
        count
    }

    fn are_parens_match(open: char, close: char) -> bool {
        match (open, close) {
            ('(', ')') | ('[', ']') | ('{', '}') => true,
            _ => false,
        }
    }

    fn get_matching_paren(c: char) -> Option<char> {
        match c {
            '(' => Some(')'),
            '[' => Some(']'),
            '{' => Some('}'),
            ')' => Some('('),
            ']' => Some('['),
            '}' => Some('{'),
            _ => None,
        }
    }
}

impl<'a> std::iter::Iterator for Lexer<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let dist_from_prev = self.ignore_whitespaces();

        let tok = self.curr.next().map(|c| match c {
            c if ['(', '{', '['].contains(&c) => self.tokenize_open_paren(c),
            c if [')', '}', ']'].contains(&c) => self.tokenize_close_paren(c),
            '0'..='9' => self.tokenize_num(c),
            '.' => match self.curr.peek() {
                Some(&c) if c.is_digit(10) => self.tokenize_num('.'),
                _ => self.tokenize_punc('.'),
            },
            c @ ('"' | '\'') => self.tokenize_quote(c, "".to_owned()),
            c if c != '_' && c.is_ascii_punctuation() => self.tokenize_punc(c),
            c if !c.is_ascii_whitespace() => self.tokenize_id(c),
            _ => unreachable!(),
        });

        let count = self.curr.get_count();
        self.col += count;
        self.codepoint += count;

        tok
            .map(|tok| tok
                .map(|tok| Token::new(
                    tok.kind,
                    Span {
                        line: self.line,
                        col: self.col - count..self.col,
                        codepoint: self.codepoint - count..self.codepoint,
                        dist_from_prev,
                    },
                ))
                .map_err(|err| Error::new(
                    err.kind,
                    Span {
                        line: self.line,
                        col: self.col - count..self.col,
                        codepoint: self.codepoint - count..self.codepoint,
                        dist_from_prev,
                    },
                ))
            )
    }
}
