use std::collections::HashSet;
use token::*;

#[cfg(test)]
mod tests;

mod expr;
pub use expr::*;

mod rule;
pub use rule::*;

mod error;
pub use error::*;

pub type SynResult<T> = Result<T, Error>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum StackItem {
    Token(Token),
    Expr(Expr),
    Err(Error),
}

impl StackItem {
    pub fn span(&self) -> &Span {
        match &self {
            &Self::Token(Token { span, .. })
            | &Self::Expr(Expr { span, .. })
            | &Self::Err(Error { span, .. }) => {
                span
            }
        }
    }

    pub fn token(&self) -> &Token {
        match self {
            Self::Token(ref token) => token,
            _ => panic!("not a token"),
        }
    }

    pub fn token_mut(&mut self) -> &mut Token {
        match self {
            Self::Token(ref mut token) => token,
            _ => panic!("not a token"),
        }
    }

    pub fn expr(&self) -> &Expr {
        match self {
            Self::Expr(ref expr) => expr,
            _ => panic!("not an expr"),
        }
    }

    pub fn expr_mut(&mut self) -> &mut Expr {
        match self {
            Self::Expr(ref mut expr) => expr,
            _ => panic!("not an expr"),
        }
    }

    pub fn err(&self) -> &Error {
        match self {
            Self::Err(ref err) => err,
            _ => panic!("not an error"),
        }
    }

    pub fn err_mut(&mut self) -> &mut Error {
        match self {
            Self::Err(ref mut err) => err,
            _ => panic!("not an error"),
        }
    }

    pub fn token_or_err(self) -> Result<Token, Error> {
        match self {
            Self::Token(token) => Ok(token),
            Self::Err(err) => Err(err),
            _ => panic!("not a token or error"),
        }
    }

    pub fn expr_or_err(self) -> Result<Expr, Error> {
        match self {
            Self::Expr(expr) => Ok(expr),
            Self::Err(err) => Err(err),
            _ => panic!("not an expr or error"),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: std::iter::Peekable<lexer::Lexer<'a>>,
    pub rules: HashSet<Rule>,

    stack: Vec<StackItem>,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Parser<'a> {
        Parser {
            lexer: lexer::Lexer::new(code).peekable(),
            rules: [].into(),
            stack: vec![],
        }
    }

    fn parse_term(token: Result<Token, lexer::Error>) -> StackItem {
        match token {
            Ok(Token {
                value: TokenKind::Lit(lit),
                span
            }) => StackItem::Expr(Expr::new(ExprKind::Lit(lit), span)),
            Ok(Token {
                value: TokenKind::Id(id),
                span
            }) => StackItem::Expr(Expr::new(ExprKind::Id(id), span)),
            Err(lexer::Error { value, span }) => StackItem::Err(Error::new(value.into(), span)),
            Ok(tok) => StackItem::Token(tok),
        }
    }

    pub fn parse(&mut self) {
        loop {
            // get next item
            let next = match self.lexer.next() {
                Some(next) => Some(Self::parse_term(next)),
                None => None,
            };

            // shiftables
            let shiftables = if let Some(next) = &next {
                let next_stack = self.stack
                    .iter()
                    .chain(std::iter::once(next))
                    .collect::<Vec<_>>();

                self.rules
                    .iter()
                    .filter(|&rule| (0..next_stack.len())
                        .rev()
                        .take(rule.patt.len())
                        .rev()
                        .any(|i| next_stack[i..]
                            .iter()
                            .zip(rule.patt.iter())
                            .all(|(&a, b)| b.match_item(a))
                        )
                    )
                    .collect::<HashSet<_>>()
            } else {
                [].into()
            };
            
            // reduce
            let mut reduced = false;
            loop {
                let curr_stack = &self.stack;
                let reducibles = self.rules
                    .iter()
                    .filter(|&rule| curr_stack
                        .iter()
                        .skip(match curr_stack.len().cmp(&rule.patt.len()) {
                            std::cmp::Ordering::Less => return false,
                            _ => curr_stack.len() - rule.patt.len(),
                        })
                        .zip(rule.patt.iter())
                        .all(|(a, b)| b.match_item(a))
                    )
                    .collect::<HashSet<_>>();

                // debug
                println!("{:?} {}", shiftables, shiftables.len());
                println!("RE: {:?}\n", reducibles);

                match reducibles.len() {
                    0 => {
                        reduced = reduced || false;
                        break;
                    },
                    1 => {
                        let rule = reducibles.into_iter().next().unwrap();
                        let skip = self.stack.len() - rule.patt.len();

                        let start = self.stack.iter().skip(skip).next().unwrap();
                        let end = self.stack.last().unwrap();

                        let span = Span::new(
                            start.span().line,
                            start.span().col.start..end.span().col.end,
                            start.span().codepoint.start..end.span().codepoint.end,
                            start.span().dist_from_prev,
                        );

                        let expr = Expr::new(
                            (rule.to_expr)(&self.stack[skip..]),
                            span,
                        );

                        self.stack.splice(skip.., std::iter::once(StackItem::Expr(expr)));
                        reduced = true;
                    },
                    _ => panic!("reduce-reduce conflict: {reducibles:#?}"),
                }
            };

            if shiftables.is_empty() && next.is_none() && !reduced {
                panic!("no pattern found");
            } else if shiftables.is_empty() && next.is_none() {
                break;
            } else {
                self.stack.push(next.unwrap());
            }
        }
    }
}

impl<'a> From<lexer::Lexer<'a>> for Parser<'a> {
    fn from(lexer: lexer::Lexer) -> Parser {
        Parser {
            lexer: lexer.peekable(),
            rules: [].into(),
            stack: vec![],
        }
    }
}
