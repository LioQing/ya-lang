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

#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: std::iter::Peekable<lexer::Lexer<'a>>,

    stack: Vec<StackItem>,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Parser<'a> {
        Parser {
            lexer: lexer::Lexer::new(code).peekable(),
            stack: vec![],
        }
    }

    fn token_to_expr(token: Result<Token, lexer::Error>) -> Option<Result<Expr, Error>> {
        match token {
            Ok(Token { value, span }) => match value {
                TokenKind::Lit(lit) => Some(Ok(Expr::new(ExprKind::Lit(lit), span))),
                TokenKind::Id(id) => Some(Ok(Expr::new(ExprKind::Id(id), span))),
                _ => None,
            },
            Err(lexer::Error { value, span }) => Some(Err(Error::new(value.into(), span))),
        }
    }

    fn shift(&mut self) {
        match self.lexer.next() {
            Some(tok) => match Self::token_to_expr(tok.clone()) {
                Some(expr) => self.stack.push(StackItem::Expr(expr)),
                None => self.stack.push(StackItem::Token(tok)),
            },
            None => unreachable!(),
        }
    }

    fn pr_reduce(&mut self) -> Option<Result<Expr, Error>> {
        None
    }
}

impl<'a> std::iter::Iterator for Parser<'a> {
    type Item = Result<Expr, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match (&self.stack[..], self.lexer.peek()) {
                // reduce cases
                
                (
                    &[_, _, ..],
                    None,
                ) => {
                    return match self.pr_reduce() {
                        Some(expr) => Some(expr),
                        _ => Some(Err(Error::new_value(ErrorKind::UnknownSyntax))),
                    }
                }
    
                // default cases
    
                ( // nothing in stack, followed by nothing -> None
                    &[],
                    None,
                ) => {
                    return None
                },
                ( // one expr in stack, followed by anything -> return
                    &[StackItem::Expr(_)],
                    _,
                ) => {
                    match self.stack.pop().unwrap() {
                        StackItem::Expr(expr) => return Some(expr),
                        _ => unreachable!(),
                    }
                },
                ( // anything in stack, followed by something -> shift
                    _,
                    Some(_),
                ) => {
                    self.shift()
                },

                // Syntax Error

                (
                    &[StackItem::Token(_)],
                    None,
                ) => {
                    return match self.stack.pop().unwrap() {
                        StackItem::Token(
                            Ok(Token { span, .. })
                            | Err(lexer::Error { span, .. })
                        ) => {
                                Some(Err(Error::new(ErrorKind::UnknownSyntax, span)))
                        },
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}

impl<'a> From<lexer::Lexer<'a>> for Parser<'a> {
    fn from(lexer: lexer::Lexer) -> Parser {
        Parser {
            lexer: lexer.peekable(),
            stack: vec![],
        }
    }
}
