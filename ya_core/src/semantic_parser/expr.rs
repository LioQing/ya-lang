use itertools::{
    Itertools,
    EitherOrBoth::*,
};

use super::*;

pub trait ParseSynExpr {
    type SynExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr
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

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        match expr {
            syn::Expr::Let(expr) => LetExpr::parse(envs, expr),
            syn::Expr::Lit(expr) => LitExpr::parse(envs, expr),
            syn::Expr::VarName(expr) => VarExpr::parse(envs, expr),
            syn::Expr::Block(expr) => BlockExpr::parse(envs, expr),
            syn::Expr::Tuple(expr) => TupleExpr::parse(envs, expr),
            syn::Expr::Call(expr) => CallExpr::parse(envs, expr),
            syn::Expr::BinOp(expr) => BinOpExpr::parse(envs, expr),
            _ => unimplemented!(),
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
    BinOp(BinOpExpr),
    UnOp(UnOpExpr),
    Func(FuncExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub var: String,
}

impl ParseSynExpr for LetExpr {
    type SynExpr = syn::LetExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let errs = vec![];

        let var = expr.var.name.clone();

        envs.envs
            .last_mut()
            .expect("No environment found").vars
            .insert(var.clone(), None);

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

    fn parse(_envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
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

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let ty = envs.get_def_var(expr.name.as_str())
            .map_err(|err| errs.push(err))
            .map_or(Type::PrimType(PrimType::Unit), |ty| ty.clone());

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
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}

impl ParseSynExpr for BlockExpr {
    type SynExpr = syn::BlockExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let stmts = expr.stmts
            .iter()
            .map(|stmt| Expr::parse(envs, stmt))
            .collect();
        
        let expr = expr.expr
            .as_ref()
            .map(|expr| Box::new(Expr::parse(envs, expr.as_ref())));

        let ty = expr
            .as_ref()
            .map_or(Type::PrimType(PrimType::Unit), |expr| expr.ty.clone());

        Expr {
            ty,
            kind: ExprKind::Block(Self {
                stmts,
                expr,
            }),
            errs: vec![],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
}

impl ParseSynExpr for TupleExpr {
    type SynExpr = syn::TupleExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let errs = vec![];

        let items = expr.items
            .iter()
            .map(|item| Expr::parse(envs, item))
            .collect::<Vec<_>>();

        let ty = Type::Tuple(items
            .iter()
            .map(|item| item.ty.clone())
            .collect());

        Expr {
            ty,
            kind: ExprKind::Tuple(Self {
                items,
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

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
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
pub struct BinOpExpr {
    pub op: String,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl ParseSynExpr for BinOpExpr {
    type SynExpr = syn::BinOpExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let op = expr.op.op.clone();
        let mut lhs = Box::new(Expr::parse(envs, &*expr.lhs));
        let rhs = Box::new(Expr::parse(envs, &*expr.rhs));

        // special case: let expression on lhs
        match (op.as_str(), &mut *lhs) {
            ("=", ref mut expr) => {
                if let ExprKind::Let(ref mut let_expr) = &mut expr.kind {
                    let let_ty = envs.get_var_mut(let_expr.var.as_str())
                        .expect("let expression did not push to environment stack");

                    *let_ty = Some(rhs.ty.clone());
                }

                expr.ty = rhs.ty.clone();
            },
            _ => {},
        };

        let bin_op = BinOp {
            op: op.clone(),
            lhs: lhs.ty.clone(),
            rhs: rhs.ty.clone(),
        };

        // special case: "=" and "." operators
        let temp_op_info;
        let prec = match op.as_str() {
            "=" => match lhs.ty.eq(&rhs.ty) {
                true => {
                    temp_op_info = OpInfo { prec: 0xf, ty: lhs.ty.clone(), assoc: OpAssoc::Ltr };
                    Ok(&temp_op_info)
                },
                false => {
                    Err(Error::AssignmentMismatchedOperandTypes {
                        lhs: lhs.ty.clone(),
                        rhs: rhs.ty.clone(),
                    })
                },
            },
            _ => envs.get_bin_op_prec(&bin_op),
        };

        let ty = prec
            .map_err(|err| errs.push(err))
            .map_or(Type::PrimType(PrimType::Unit), |op| match &rhs.kind {
                // TODO: implement operator precedence
                _ => op.ty.clone(),
            });

        Expr {
            ty,
            kind: ExprKind::BinOp(BinOpExpr {
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
pub struct UnOpExpr {
    pub op: String,
    pub op_pos: UnaryOpPos,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncExpr {
    pub ty: FuncType,
    pub id: usize,
}