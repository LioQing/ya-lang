use super::*;

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub expr_ty: ExprType,
    pub value_ty: token::TypeName,
}

#[derive(Debug, PartialEq)]
pub enum ExprType {
    /** no operation: `;` */
    NoOp,

    /** block: `{ %(%expr);* }` */
    Block(ExprBlock),
}

#[derive(Debug, PartialEq)]
pub struct ExprBlock {
    pub exprs: Vec<Expr>,
}
