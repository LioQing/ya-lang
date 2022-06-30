use std::collections::HashSet;
use token::*;

#[cfg(test)]
mod tests;

pub mod expr;
pub use expr::*;

pub mod error;
pub use error::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum StackItem {
    Token(Result<Token, lexer::Error>),
    Expr(Result<Expr, Error>),
}

impl StackItem {
    pub fn eq_patt(&self, other: &StackItem) -> bool {
        match (&self, &other) {
            (
                &Self::Token(Ok(Token { value: a, .. })),
                &Self::Token(Ok(Token { value: b, .. }))
            ) => {
                a.eq_patt(b)
            },
            (
                &Self::Expr(Ok(Expr { value: a, .. })),
                &Self::Expr(Ok(Expr { value: b, .. })),
            ) => {
                a.eq_patt(b)
            },
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: std::iter::Peekable<lexer::Lexer<'a>>,
    pub patts: HashSet<Vec<StackItem>>,

    stack: Vec<StackItem>,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Parser<'a> {
        Parser {
            lexer: lexer::Lexer::new(code).peekable(),
            patts: [].into(),
            stack: vec![],
        }
    }

    fn token_to_stack_item(token: Result<Token, lexer::Error>) -> StackItem {
        match token {
            Ok(Token {
                value: TokenKind::Lit(lit),
                span
            }) => StackItem::Expr(Ok(Expr::new(ExprKind::Lit(lit), span))),
            Ok(Token {
                value: TokenKind::Id(id),
                span
            }) => StackItem::Expr(Ok(Expr::new(ExprKind::Id(id), span))),
            tok => StackItem::Token(tok),
        }
    }

    fn parse(&mut self) {
        loop {
            // get next item
            let next = match self.lexer.next() {
                Some(next) => Some(Self::token_to_stack_item(next)),
                None => None,
            };

            // shiftables
            let shiftables = if let Some(next) = &next {
                let next_stack = self.stack
                    .iter()
                    .chain(std::iter::once(next))
                    .collect::<Vec<_>>();

                self.patts
                    .iter()
                    .filter(|&patt| (0..next_stack.len())
                        .rev()
                        .take(patt.len())
                        .rev()
                        .any(|i| next_stack[i..]
                            .iter()
                            .zip(patt.iter())
                            .all(|(&a, b)| a.eq_patt(b))
                        )
                    )
                    .collect::<HashSet<_>>()
            } else {
                [].into()
            };
            
            // reduce
            let curr_stack = &self.stack;

            let reducibles = self.patts
                .iter()
                .filter(|&patt| curr_stack
                    .iter()
                    .skip(match curr_stack.len().cmp(&patt.len()) {
                        std::cmp::Ordering::Greater => curr_stack.len() - patt.len(),
                        std::cmp::Ordering::Equal => 0,
                        std::cmp::Ordering::Less => return false,
                    })
                    .zip(patt.iter())
                    .all(|(a, b)| a.eq_patt(b))
                )
                .collect::<HashSet<_>>();

            // debug
            println!("{:?} {}", shiftables, shiftables.len());
            println!("RE: {:?}\n", reducibles);

            if shiftables.is_empty() {
                match reducibles.len() {
                    0 => panic!("no pattern found"),
                    1 => (), // reduce
                    _ => panic!("reduce-reduce conflict: {reducibles:#?}"),
                }

                if next.is_none() {
                    break;
                }
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
            patts: [].into(),
            stack: vec![],
        }
    }
}
