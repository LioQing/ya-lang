use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StmtsKind {
    pub expr: SynResult<ExprKind>,
    pub next: Option<Box<Stmts>>,
}

pub type Stmts = Spanned<StmtsKind>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StmtKind {
    pub expr: ExprKind,
}

impl From<ExprKind> for StmtKind {
    fn from(expr: ExprKind) -> Self {
        Self { expr }
    }
}

pub type Stmt = Spanned<StmtKind>;

pub trait IntoStmt {
    fn into_stmt(self) -> Stmt;
}

impl IntoStmt for Expr {
    fn into_stmt(self) -> Stmt {
        Stmt::new(
            StmtKind { expr: self.value },
            self.span,
        )
    }
}