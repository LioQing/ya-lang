use itertools::{
    Itertools,
    EitherOrBoth::*,
};

use super::*;

pub trait ParseSynExpr where Self: Sized {
    type SynExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr;

    fn parse_with_local_vars(envs: &mut EnvStack, expr: &Self::SynExpr, local_vars: HashMap<String, Option<Type>>) -> Expr {
        envs.envs.push(Env {
            tys: HashMap::new(),
            vars: local_vars,
            bin_ops: HashMap::new(),
            un_ops: HashMap::new(),
        });

        let expr = Self::parse(envs, expr);

        envs.envs.pop();

        expr
    }
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
            syn::Expr::Lit(lit) => Ok(Type::Prim(<Result<PrimType, Error>>::from(lit)?)),
            syn::Expr::Func(func) => Ok(Type::Func(func.into())),
            syn::Expr::VarName(name) => Ok(envs.get_def_var(name.name.as_str())?.clone()),
            _ => Ok(Type::Prim(PrimType::Unit)),
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
            syn::Expr::UnOp(expr) => UnOpExpr::parse(envs, expr),
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
            ty: Type::Prim(PrimType::Unit),
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
            Ok(ty) => Type::Prim(ty),
            Err(err) => {
                errs.push(err);
                Type::Prim(PrimType::Unit)
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
            .map_or(Type::Prim(PrimType::Unit), |ty| ty.clone());

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
            .map_or(Type::Prim(PrimType::Unit), |expr| expr.ty.clone());

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

impl BinOpExpr {
    fn parse_recur(
        envs: &mut EnvStack,
        mut errs: Vec<Error>,
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> Expr {
        let (lhs_op_info, rhs_op_info) =
            if let Some(infos) = Self::flatten_op_info(envs, &mut errs, &op, &lhs, &rhs) {
                infos
            } else {
                return Expr {
                    ty: Type::Prim(PrimType::Unit),
                    kind: ExprKind::BinOp(Self {
                        op,
                        lhs,
                        rhs,
                    }),
                    errs,
                };
            };

        // if lhs precedence less than rhs precedence, set lhs's rhs to rhs's lhs, and rhs's lhs to lhs
        if lhs_op_info.prec < rhs_op_info.prec {
            // break down rhs_op into smaller parts
            let BinOpExpr {
                op: rhs_expr_op,
                lhs: rhs_expr_lhs,
                rhs: rhs_expr_rhs
            } = match rhs.kind {
                ExprKind::BinOp(bin_expr) => bin_expr,
                _ => unreachable!(),
            };

            // create the new lhs
            let new_lhs = Box::new(Expr {
                ty: Self::get_ty(envs, &mut errs, &op, &lhs, &rhs_expr_lhs),
                kind: ExprKind::BinOp(BinOpExpr {
                    op,
                    lhs,
                    rhs: rhs_expr_lhs,
                }),
                errs,
            });

            let mut errs = vec![];
            let ty = Self::get_ty(envs, &mut errs, &rhs_expr_op, &new_lhs, &rhs_expr_rhs);

            Self::new_expr(ty, rhs_expr_op, new_lhs, rhs_expr_rhs, errs)
        } else {
            Self::new_expr(lhs_op_info.ty, op, lhs, rhs, errs)
        }
    }

    fn parse_base(
        envs: &mut EnvStack,
        mut errs: Vec<Error>,
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> Expr {
        Self::new_expr(
            Self::get_ty(envs, &mut errs, &op, &lhs, &rhs),
            op, lhs, rhs, errs,
        )
    }

    ///     lhs_op            
    ///     /    \               lhs_op   rhs_op
    ///    /      \              /    \   /    \
    ///  op1     rhs_op    =>   /      \ /      \
    ///          /    \       op1      op2      op3
    ///         /      \
    ///       op2      op3     
    fn flatten_op_info(
        envs: &mut EnvStack,
        errs: &mut Vec<Error>,
        op: &String,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
    ) -> Option<(OpInfo, OpInfo)> {
        let rhs_bin_expr = match &rhs.as_ref().kind {
            ExprKind::BinOp(bin_expr) => bin_expr,
            _ => unreachable!(),
        };

        let lhs_bin_op = BinOp {
            op: op.clone(),
            lhs: lhs.ty.clone(),
            rhs: rhs_bin_expr.lhs.ty.clone(),
        };

        let rhs_bin_op = BinOp {
            op: rhs_bin_expr.op.clone(),
            lhs: rhs_bin_expr.lhs.ty.clone(),
            rhs: rhs_bin_expr.rhs.ty.clone(),
        };

        let (lhs_bin_op_info, rhs_bin_op_info) = match (
            envs.get_bin_op(&lhs_bin_op),
            envs.get_bin_op(&rhs_bin_op),
        ) {
            (Err(err1), Err(err2)) => {
                errs.push(err1);
                errs.push(err2);
                return None;
            },
            (Err(err), _) | (_, Err(err)) => {
                errs.push(err);
                return None;
            },
            (Ok(info1), Ok(info2)) => (info1, info2),
        };

        Some((lhs_bin_op_info, rhs_bin_op_info))
    }

    fn get_ty(
        envs: &mut EnvStack,
        errs: &mut Vec<Error>,
        op: &String,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
    ) -> Type {
        let bin_op = BinOp {
            op: op.clone(),
            lhs: lhs.ty.clone(),
            rhs: rhs.ty.clone(),
        };

        envs.get_bin_op(&bin_op)
            .map_err(|err| errs.push(err))
            .map_or(Type::Prim(PrimType::Unit), |op| op.ty.clone())
    }

    fn new_expr(ty: Type, op: String, lhs: Box<Expr>, rhs: Box<Expr>, errs: Vec<Error>) -> Expr {
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

impl ParseSynExpr for BinOpExpr {
    type SynExpr = syn::BinOpExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let errs = vec![];

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

        match &*rhs {
            Expr { kind: ExprKind::BinOp(_), .. } => {
                Self::parse_recur(envs, errs, op, lhs, rhs)
            },
            _ => Self::parse_base(envs, errs, op, lhs, rhs),
        }
    }
}

impl From<syn::UnOpPos> for UnOpPos {
    fn from(pos: syn::UnOpPos) -> Self {
        if pos == syn::UnOpPos::Pre { UnOpPos::Pre } else { UnOpPos::Suf }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnOpExpr {
    pub op: String,
    pub op_pos: UnOpPos,
    pub expr: Box<Expr>,
}

impl ParseSynExpr for UnOpExpr {
    type SynExpr = syn::UnOpExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let op  = expr.op.op.clone();
        let op_pos: UnOpPos = expr.op_pos.into();
        let expr = Box::new(Expr::parse(envs, &*expr.expr));

        let ty = envs.get_un_op(&UnOp {
            op: op.clone(),
            op_pos: op_pos.clone(),
            ty: expr.ty.clone(),
        })
        .map_err(|err| errs.push(err))
        .map_or(Type::Prim(PrimType::Unit), |ty| ty.clone());

        Expr {
            ty,
            kind: ExprKind::UnOp(UnOpExpr {
                op,
                op_pos,
                expr,
            }),
            errs,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncExpr {
    pub ty: FuncType,
    pub id: usize,
}

// impl ParseSynExpr for FuncExpr {
//     type SynExpr = syn::FuncExpr;

//     fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {

//         Expr {
//             ty,
//             kind: ExprKind::Func(FuncExpr {
//             }),
//             errs: vec![],
//         }
//     }
// }