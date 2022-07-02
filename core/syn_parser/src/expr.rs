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

    /** <bin> = <expr> <punc as bin op> <expr> */
    Bin(BinExpr),
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
    pub id: SynResult<Spanned<String>>,
    pub ty: SynResult<Option<Spanned<String>>>,
    pub expr: Option<Box<Result<Expr, Error>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ConstExpr {
    pub id: SynResult<Spanned<String>>,
    pub ty: SynResult<Spanned<String>>,
    pub expr: Box<Result<Expr, Error>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct BinExpr {
    pub lhs: Box<Result<Expr, Error>>,
    pub op: SynResult<Spanned<String>>,
    pub rhs: Box<Result<Expr, Error>>,
}
