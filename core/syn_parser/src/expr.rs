use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ExprKind {
    /** <lit> */
    Lit(LitToken),

    /** <id> */
    Id(String),

    /** <paren> = (<expr>) */
    Paren(ParenExpr),

    /** <block> = { [<expr>];* [<expr>]? } */
    Block(BlockExpr),

    /** <let> = let [mut]? <id> [: <ty>]? [= <expr>]? */
    Let(LetExpr),

    /** <const> = const <id>: <ty> = <const expr> */
    Const(ConstExpr),

    /** <fn> = ([[mut]? <id>: <ty>],*) [-> <ty>]? [<block> | => <expr>] */
    Fn(FnExpr),
}

pub type Expr = Spanned<ExprKind>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ParenExpr {
    pub expr: Box<Result<Expr, Error>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct BlockExpr {
    pub stmts: Vec<Result<Expr, Error>>,
    pub expr: Option<Box<Result<Expr, Error>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LetExpr {
    pub mutable: bool,
    pub id: String,
    pub ty: Option<String>,
    pub expr: Option<Box<Result<Expr, Error>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ConstExpr {
    pub id: String,
    pub ty: String,
    pub expr: Box<Result<Expr, Error>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FnExpr {
    pub args: Vec<(String, String)>,
    pub ret_ty: Option<String>,
    pub block: BlockExpr,
    pub expr: Option<Box<Result<Expr, Error>>>,
}
