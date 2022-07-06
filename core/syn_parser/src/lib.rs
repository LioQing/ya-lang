use std::{collections::HashSet, hash::Hash};
use token::*;

#[cfg(test)]
mod tests;

mod expr;
pub use expr::*;

mod stmts;
pub use stmts::*;

mod rule;
pub use rule::*;

mod error;
pub use error::*;

mod stack_item;
pub use stack_item::*;

pub type SynResult<T> = Result<T, Error>;
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
                    );

                let max_prec = reducibles
                    .clone()
                    .map(|rule| rule.prec)
                    .max()
                    .unwrap_or(0);
                
                let reducibles = reducibles
                    .filter(|&rule| rule.prec == max_prec)
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

                        let item = (rule.reduce)(&self.stack[skip..]);

                        self.stack.splice(skip.., std::iter::once(item));
                        reduced = true;
                    },
                    _ => panic!("reduce-reduce conflict: {reducibles:#?}"),
                }
            };

            if shiftables.is_empty() && next.is_none() && !reduced {
                let skip = self.stack.len() - self.stack
                    .iter()
                    .rev()
                    .take_while(|&item| !matches!(item, &StackItem::Expr(_)))
                    .count();
                    
                let span = self.stack[skip].span().merge(self.stack.last().unwrap().span());

                self.stack.splice(skip.., std::iter::once(StackItem::Err(Error::new(
                    ErrorKind::UnknownSyntax,
                    span,
                ))));
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
