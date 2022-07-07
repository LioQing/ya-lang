use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StmtsKind {
    pub expr: Result<ExprKind, ErrorKind>,
    pub next: Option<Box<Stmts>>,
}

impl StmtsKind {
    pub fn push(&mut self, stmt: SynResult<Stmt>) {
        let mut curr = self;
        while curr.next.is_some() {
            curr = &mut curr.next.as_mut().unwrap().as_mut().value;
        }

        let (expr, span) = match stmt {
            Ok(stmt) => (Ok(stmt.value.expr), stmt.span),
            Err(err) => (Err(err.value), err.span),
        };

        curr.next = Some(Box::new(Stmts {
            value: StmtsKind {
                expr,
                next: None,
            },
            span,
        }));
    }
}

pub type Stmts = Spanned<StmtsKind>;

pub trait StmtsIntoExprVecExt {
    fn into_expr_vec(self) -> Vec<SynResult<Expr>>;
}

impl StmtsIntoExprVecExt for Stmts {
    fn into_expr_vec(self) -> Vec<SynResult<Expr>> {
        self
            .into_iter()
            .map(|stmt| stmt
                .map(|stmt| Expr::new(stmt.value.expr, stmt.span))
            )
            .collect()
    }
}

pub struct StmtsIntoIter {
    curr: Option<Stmts>,
}

pub trait StmtsIntoIterExt {
    fn into_iter(self) -> StmtsIntoIter;
}

impl StmtsIntoIterExt for Stmts {
    fn into_iter(self) -> StmtsIntoIter {
        StmtsIntoIter {
            curr: Some(self),
        }
    }
}

impl Iterator for StmtsIntoIter {
    type Item = SynResult<Stmt>;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr.take()?;
        let res = match curr.value.expr {
            Ok(expr) => Ok(Stmt::new(expr.into(), curr.span)),
            Err(err) => Err(Error::new(err, curr.span)),
        };

        self.curr = curr.value.next.map(|next| *next);
        
        Some(res)
    }
}

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

pub trait IntoStmtExt {
    fn into_stmt(self) -> Stmt;
}

impl IntoStmtExt for Expr {
    fn into_stmt(self) -> Stmt {
        Stmt::new(
            StmtKind { expr: self.value },
            self.span,
        )
    }
}