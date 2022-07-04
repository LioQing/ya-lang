use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StmtsKind {
    pub expr: ExprKind,
    pub next: Option<Box<Result<Stmts, Error>>>,
}

pub type Stmts = Spanned<StmtsKind>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub  struct StmtKind {
    pub expr: ExprKind,
}

pub type Stmt = Spanned<StmtKind>;