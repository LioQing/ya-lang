use super::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
}

impl Expr {
    pub fn get_ty_from_syn(expr: &syn::Expr) -> Result<Type, Error> {
        match expr {
            syn::Expr::Let(_) => Ok(Type::PrimType(PrimType::Unit)),
            syn::Expr::Func(ref func) => Ok(Type::Func(func.into())),
            syn::Expr::Lit(ref lit) => Ok(Type::PrimType(<Result<PrimType, Error>>::from(lit)?)),
            _ => Ok(Type::PrimType(PrimType::Unit)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Let(LetExpr),
    Lit(LitExpr),
    Var(VarExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
    Call(CallExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Func(FuncExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub var: String,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LitKind {
    String,
    Char,
    Integer,
    Float,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LitExpr {
    pub value: String,
    pub prefix: String,
    pub suffix: String,
    pub kind: LitKind,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarExpr {
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleExpr {
    pub vars: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    pub caller: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub op: String,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOpPos {
    Pre,
    Post,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub op: String,
    pub op_pos: UnaryOpPos,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncExpr {
    pub ty: FuncType,
    pub body: Box<BlockExpr>,
}