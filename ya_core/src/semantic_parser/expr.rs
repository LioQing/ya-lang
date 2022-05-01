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
    pub var: Var,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LitExpr {
    
}

#[derive(Debug, PartialEq, Clone)]
pub enum LitKind {
    
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarExpr {
    
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleExpr {
    
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    
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