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
    fn get_op_info(
        envs: &mut EnvStack,
        errs: &mut Vec<Error>,
        op: &String,
        lhs: &Expr,
        rhs: &Expr,
    ) -> OpInfo {
        let bin_op = BinOp {
            op: op.clone(),
            lhs: lhs.ty.clone(),
            rhs: rhs.ty.clone(),
        };

        envs.get_bin_op(&bin_op)
            .map_err(|err| errs.push(err))
            .unwrap_or(OpInfo::new(Type::Prim(PrimType::Unit), 0))
    }
}

impl ParseSynExpr for BinOpExpr {
    type SynExpr = syn::BinOpExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        struct OpFlatInfo {
            info: OpInfo,
            op: String,
            errs: Vec<Error>,
            lhs: usize,
            rhs: usize,
        }

        let mut op_flat_infos = vec![];
        let mut operands = vec![Some(Expr::parse(envs, &*expr.lhs))];
        // push all flattened operations information into op_flat_infos
        // push all operands into operands
        {
            let mut curr_op = expr;
            let mut idx = 0;
            loop {
                let mut errs = vec![];
                let push_flat_info = |envs: &mut EnvStack, operands: &Vec<Option<Expr>>| {
                    op_flat_infos.push(OpFlatInfo {
                        info: Self::get_op_info(
                            envs,
                            &mut errs,
                            &curr_op.op.op,
                            operands[idx].as_ref().unwrap(),
                            operands[idx + 1].as_ref().unwrap(),
                        ),
                        op: curr_op.op.op.clone(),
                        errs,
                        lhs: idx,
                        rhs: idx + 1,
                    });
                };

                match &*curr_op.rhs {
                    syn::Expr::BinOp(bin_expr) => {
                        operands.push(Some(Expr::parse(envs, &*bin_expr.lhs)));
                        push_flat_info(envs, &operands);

                        curr_op = bin_expr;
                        idx += 1;
                    },
                    _ => {
                        operands.push(Some(Expr::parse(envs, &*curr_op.rhs)));
                        push_flat_info(envs, &operands);
                        break;
                    },
                }
            }
        }

        fn resolve_prec(envs: &mut EnvStack, mut op_flat_infos: Vec<OpFlatInfo>, operands: &mut Vec<Option<Expr>>) -> Expr {
            if op_flat_infos.len() == 1 {
                let OpFlatInfo { 
                    info: OpInfo {
                        ty,
                        ..
                    },
                    op,
                    errs,
                    lhs,
                    rhs,
                } = op_flat_infos.pop().unwrap();

                return Expr {
                    ty,
                    kind: ExprKind::BinOp(BinOpExpr {
                        op,
                        lhs: Box::new(std::mem::replace(&mut operands[lhs], None).unwrap()),
                        rhs: Box::new(std::mem::replace(&mut operands[rhs], None).unwrap()),
                    }),
                    errs,
                };
            }

            let root = op_flat_infos
                .iter()
                .enumerate()
                .rev()
                .max_by(|(_, a), (_, b)| a.info.prec.cmp(&b.info.prec))
                .unwrap();
            
            let root = if root.1.info.assoc == OpAssoc::Right {
                op_flat_infos
                    .iter()
                    .enumerate()
                    .max_by(|(_, a), (_, b)| a.info.prec.cmp(&b.info.prec))
                    .map(|(idx, _)| idx)
                    .unwrap()
            } else {
                root.0
            };

            let rhs_vec = op_flat_infos.split_off(root + 1);
            let mut lhs_vec = op_flat_infos;
            let mut root = lhs_vec.pop().unwrap();

            let rhs = match rhs_vec.is_empty() {
                true => std::mem::replace(&mut operands[root.rhs], None).unwrap(),
                false => resolve_prec(envs, rhs_vec, operands),
            };
            let lhs = match lhs_vec.is_empty() {
                true => std::mem::replace(&mut operands[root.lhs], None).unwrap(),
                false => resolve_prec(envs, lhs_vec, operands),
            };

            Expr {
                ty: BinOpExpr::get_op_info(envs, &mut root.errs, &root.op, &lhs, &rhs).ty.clone(),
                kind: ExprKind::BinOp(BinOpExpr {
                    op: root.op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }),
                errs: root.errs,
            }
        }

        resolve_prec(envs, op_flat_infos, &mut operands)
    }
}

impl From<syn::UnOpPos> for UnOpPos {
    fn from(pos: syn::UnOpPos) -> Self {
        if pos == syn::UnOpPos::Pre { UnOpPos::Pre } else { UnOpPos::Suf }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnOpExpr {
    pub op: char,
    pub op_pos: UnOpPos,
    pub expr: Box<Expr>,
}

impl ParseSynExpr for UnOpExpr {
    type SynExpr = syn::UnOpExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let op  = expr.op;
        let op_pos: UnOpPos = expr.op_pos.into();
        let expr = Box::new(Expr::parse(envs, &*expr.expr));

        let ty = envs.get_un_op(&UnOp {
            op,
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