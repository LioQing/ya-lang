use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StmtKind {
    pub expr: ExprKind,
}

pub type Stmt = Spanned<StmtKind>;