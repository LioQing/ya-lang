use itertools::{
    Itertools,
    EitherOrBoth::*,
};

use super::*;

pub trait ParseSynExpr {
    type SynExpr;

    fn parse(envs: &EnvStack, expr: &Self::SynExpr) -> Expr
    where
        Self: Sized;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
    pub errs: Vec<Error>,
}

impl Expr {
    pub fn get_ty_from_syn(envs: &EnvStack, expr: &syn::Expr) -> Result<Type, Error> {
        match expr {
            syn::Expr::Lit(lit) => Ok(Type::PrimType(<Result<PrimType, Error>>::from(lit)?)),
            syn::Expr::Func(func) => Ok(Type::Func(func.into())),
            syn::Expr::VarName(name) => Ok(envs.get_def_var(name.name.as_str())?.clone()),
            _ => Ok(Type::PrimType(PrimType::Unit)),
        }
    }
}

impl ParseSynExpr for Expr {
    type SynExpr = syn::Expr;

    fn parse(envs: &EnvStack, expr: &Self::SynExpr) -> Expr {
        // match expr {

        // }

        Expr {
            ty: Type::PrimType(PrimType::Unit),
            kind: ExprKind::Tuple(TupleExpr { items: vec![] }),
            errs: vec![],
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
}

impl ParseSynExpr for LetExpr {
    type SynExpr = syn::LetExpr;

    fn parse(_envs: &EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let var = expr.var.name.clone();
                    
        // let deduced_ty = match expr.expr
        //     .as_ref()
        //     .map(|expr| {
        //         Expr::get_ty_from_syn(envs, expr.as_ref())
        //     })
        // {
        //     Some(Err(err)) => {
        //         errs.push(err);
        //         None
        //     },
        //     ty => ty.map(|r| r.unwrap()),
        // };
        
        // let ty = expr.ty
        //     .as_ref()
        //     .map(|ty| ty.into());

        // let ty = match (ty, deduced_ty) {
        //     (Some(ty1), Some(ty2)) if ty1 != ty2 => {
        //         errs.push(Error::ConflictingTypes { type1: ty1, type2: ty2 });
        //         None
        //     },
        //     (_, Some(ty)) | (Some(ty), _) =>  Some(ty),
        //     (None, None) => None,
        // };

        // let expr = match expr.expr {
        //     Some(expr) => Some(Box::new(Expr::parse(envs, expr.as_ref()))),
        //     None => None,
        // };

        Expr {
            ty: Type::PrimType(PrimType::Unit),
            kind: ExprKind::Let(Self {
                var,
            }),
            errs
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LitKind {
    String,
    Char,
    Integer,
    Float,
}

impl From<&syn::token::LitKind> for LitKind {
    fn from(lit: &syn::token::LitKind) -> Self {
        match lit {
            syn::token::LitKind::String => LitKind::String,
            syn::token::LitKind::Char => LitKind::Char,
            syn::token::LitKind::Integer => LitKind::Integer,
            syn::token::LitKind::Float => LitKind::Float,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LitExpr {
    pub value: String,
    pub prefix: String,
    pub suffix: String,
    pub kind: LitKind,
}

impl ParseSynExpr for LitExpr {
    type SynExpr = syn::token::Lit;

    fn parse(_envs: &EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let ty = match <Result<PrimType, Error>>::from(expr) {
            Ok(ty) => Type::PrimType(ty),
            Err(err) => {
                errs.push(err);
                Type::PrimType(PrimType::Unit)
            },
        };

        Expr {
            ty,
            kind: ExprKind::Lit(Self {
                value: expr.value.clone(),
                prefix: expr.prefix.clone(),
                suffix: expr.suffix.clone(),
                kind: (&expr.kind).into(),
            }),
            errs,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarExpr {
    pub name: String,
}

impl ParseSynExpr for VarExpr {
    type SynExpr = syn::token::VarName;

    fn parse(envs: &EnvStack, expr: &Self::SynExpr) -> Expr {
        let (errs, ty) = match envs.get_def_var(expr.name.as_str()) {
            Err(err) => (vec![err], Type::PrimType(PrimType::Unit)),
            Ok(ty) => (vec![], ty.clone()),
        };

        Expr {
            ty,
            kind: ExprKind::Var(Self {
                name: expr.name.clone(),
            }),
            errs,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
}

impl ParseSynExpr for TupleExpr {
    type SynExpr = syn::TupleExpr;

    fn parse(envs: &EnvStack, expr: &Self::SynExpr) -> Expr {
        let errs = vec![];

        let items_iter = expr.items
            .iter()
            .map(|item| Expr::parse(envs, item));

        let ty = Type::Tuple(items_iter
            .clone()
            .map(|item| item.ty)
            .collect()
        );

        Expr {
            ty,
            kind: ExprKind::Tuple(Self {
                items: items_iter.collect(),
            }),
            errs,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

impl ParseSynExpr for CallExpr {
    type SynExpr = syn::CallExpr;

    fn parse(envs: &EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let args: Vec<Expr> = expr.args
            .iter()
            .map(|arg| Expr::parse(envs, arg))
            .collect();

        let callee = Box::new(Expr::parse(envs, &*expr.callee));

        match &callee.as_ref().ty {
            Type::Func(ty) => {
                ty.params
                    .iter()
                    .zip_longest(args.iter().map(|arg| &arg.ty))
                    .for_each(|pair| match pair {
                        Both(p, a) if *p == *a => {},
                        Both(p, a) => {
                            errs.push(Error::MismatchedArgument {
                                expected: Some(p.clone()),
                                found: Some(a.clone()),
                            });
                        },
                        Left(p) => {
                            errs.push(Error::MismatchedArgument {
                                expected: Some(p.clone()),
                                found: None,
                            });
                        },
                        Right(a) => {
                            errs.push(Error::MismatchedArgument {
                                expected: None,
                                found: Some(a.clone()),
                            });
                        },
                    });
            },
            found => { errs.push(Error::ExpectedCallable { found: found.clone() }); },
        };

        Expr {
            ty: callee.ty.clone(),
            kind: ExprKind::Call(CallExpr {
                callee,
                args
            }),
            errs,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub op: String,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl ParseSynExpr for BinaryExpr {
    type SynExpr = syn::BinaryExpr;

    fn parse(envs: &EnvStack, expr: &Self::SynExpr) -> Expr {
        let op = expr.op.op.clone();
        let lhs = Box::new(Expr::parse(envs, &*expr.lhs));
        let rhs = Box::new(Expr::parse(envs, &*expr.rhs));

        let (errs, ty) = match envs.get_bin_op_prec(&BinOp {
            op: op.clone(),
            lhs: lhs.ty.clone(),
            rhs: rhs.ty.clone(),
        }) {
            Ok(op) => (vec![], op.ty.clone()),
            Err(err) => (vec![err], Type::PrimType(PrimType::Unit)),
        };

        Expr {
            ty,
            kind: ExprKind::Binary(BinaryExpr {
                op,
                lhs,
                rhs,
            }),
            errs,
        }
    }
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
    pub id: usize,
}