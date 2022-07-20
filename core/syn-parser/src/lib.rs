use std::{collections::HashSet, hash::Hash};
use token::*;

#[cfg(test)]
mod tests;

mod expr;
pub use expr::*;

mod repeats;
pub use repeats::*;

mod let_decl;
pub use let_decl::*;

mod param_decl;
pub use param_decl::*;

mod fn_decl;
pub use fn_decl::*;

mod rule;
pub use rule::*;

mod error;
pub use error::*;

mod stack_item;
pub use stack_item::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: std::iter::Peekable<lexer::Lexer<'a>>,
    pub rules: HashSet<Rule>,
    pub assoc: fn(i32) -> Assoc,

    stack: Vec<StackItem>,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Parser<'a> {
        Parser {
            lexer: lexer::Lexer::new(code).peekable(),
            rules: [].into(),
            assoc: |_| Assoc::Left,
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
            }) => StackItem::Expr(Expr::new(ExprKind::Id(IdExpr { id: Spanned::new(id, span.clone()), scope: vec![] }), span)),
            Err(lexer::Error { value, span }) => StackItem::Err(Error::new(value.into(), span)),
            Ok(tok) => StackItem::Token(tok),
        }
    }

    fn shiftables<'b>(stack: Vec<&StackItem>, rules: &'b HashSet<Rule>) -> HashSet<&'b Rule> {
        let shiftables = rules
            .iter()
            .filter(|&rule| (0..stack.len())
                .rev()
                .take(rule.patt.len())
                .rev()
                .take_while(|&i| i < stack.len() - 1)
                .any(|i| stack[i..]
                    .iter()
                    .zip(rule.patt.iter())
                    .all(|(&a, b)| b.match_item(a))
                )
            );
        
        shiftables
            .clone()
            .filter(|rule| match shiftables
                .clone()
                .map(|rule| rule.prec)
                .max()
            {
                Some(prec) => prec == rule.prec,
                None => false,
            })
            .collect::<HashSet<_>>()
    }

    pub fn parse(&mut self) {
        loop {
            // get next item
            let next = match self.lexer.next() {
                Some(next) => Some(Self::parse_term(next)),
                None => None,
            };

            let mut shiftables;
            loop {
                // shifts
                shiftables = if let Some(next) = &next {
                    let next_stack = self.stack
                        .iter()
                        .chain(std::iter::once(next))
                        .collect::<Vec<_>>();
    
                    Self::shiftables(next_stack, &self.rules)
                } else {
                    [].into()
                };

                let shift_prec = shiftables.iter().next().map(|rule| rule.prec);

                // reduces
                let curr_stack = &self.stack;
                let reducibles = self.rules
                    .iter()
                    .filter(|&rule| {
                        curr_stack
                            .iter()
                            .skip(match curr_stack.len().cmp(&rule.patt.len()) {
                                std::cmp::Ordering::Less => return false,
                                _ => curr_stack.len() - rule.patt.len(),
                            })
                            .zip(rule.patt.iter())
                            .all(|(a, b)| b.match_item(a))
                    });

                let reduce_prec = reducibles
                    .clone()
                    .map(|rule| rule.prec)
                    .max();
                
                let reducibles = reducibles
                    .filter(|&rule| match reduce_prec {
                        Some(prec) => prec == rule.prec,
                        None => false,
                    })
                    .collect::<HashSet<_>>();

                println!("{:?}", next);
                // println!("{:#?}", self.stack);
                println!("{:?}", shiftables);
                println!("{:?}", reducibles);

                match reducibles.len() {
                    0 => {
                        break;
                    },
                    1 => {
                        let rule = reducibles.into_iter().next().unwrap();

                        if shift_prec
                            .map(|prec|
                                prec > reduce_prec.unwrap()
                                || prec == reduce_prec.unwrap() && (self.assoc)(prec) == Assoc::Right
                            )
                            .unwrap_or(false)
                        {
                            break;
                        }
                        
                        let skip = self.stack.len() - rule.patt.len();
                        let item = (rule.reduce)(&self.stack[skip..], next.as_ref());

                        if let StackItem::None = item {
                            println!("skipped\n");
                            break;
                        }

                        self.stack.splice(skip.., std::iter::once(item));
                        println!("reduced\n");
                    },
                    _ => {
                        if shift_prec
                            .map(|prec|
                                prec > reduce_prec.unwrap()
                                || prec == reduce_prec.unwrap() && (self.assoc)(prec) == Assoc::Right
                            )
                            .unwrap_or(false)
                        {
                            break;
                        } else {
                            let max_len = reducibles
                                .iter()
                                .map(|&rule| rule.patt.len())
                                .max()
                                .unwrap();
                            
                            let rule = reducibles
                                .iter()
                                .filter(|&&rule| rule.patt.len() == max_len)
                                .collect::<Vec<_>>();
                            
                            let rule = if rule.len() > 1 {
                                panic!("reduce-reduce conflict:\n{rule:#?}");
                            } else {
                                **rule.first().unwrap()
                            };

                            let skip = self.stack.len() - rule.patt.len();
                            let item = (rule.reduce)(&self.stack[skip..], next.as_ref());

                            if let StackItem::None = item {
                                println!("skipped\n");
                                break;
                            }

                            self.stack.splice(skip.., std::iter::once(item));
                            println!("reduced\n");
                        }
                    },
                }
            }
            
            if shiftables.is_empty() && next.is_none() {
                break;
            } else {
                self.stack.push(next.unwrap());
                println!("shifted\n");
            }
        }
    }
}

impl<'a> From<lexer::Lexer<'a>> for Parser<'a> {
    fn from(lexer: lexer::Lexer) -> Parser {
        Parser {
            lexer: lexer.peekable(),
            rules: [].into(),
            assoc: |_| Assoc::Left,
            stack: vec![],
        }
    }
}
