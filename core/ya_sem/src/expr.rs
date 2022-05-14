use itertools::{
    Itertools,
    EitherOrBoth::*,
};

use super::*;

pub trait ParseSynExpr where Self: Sized {
    type SynExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr;

    fn parse_with_local_vars(envs: &mut EnvStack, expr: &Self::SynExpr, local_vars: HashMap<String, Option<Type>>) -> Expr {
        envs.stack.push(Env {
            tys: HashMap::new(),
            vars: local_vars,
            bin_ops: HashMap::new(),
            un_ops: HashMap::new(),
            consts: HashMap::new(),
        });

        envs.stack.push(Env::new());

        let mut expr = Self::parse(envs, expr);

        expr.env = match (envs.stack.pop(), envs.stack.pop()) {
            (Some(env_inner), Some(env_outer)) => {
                Some((env_outer, env_inner))
            },
            (None, None) => None,
            _ => unreachable!(),
        };

        expr
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
    pub errs: Vec<Error>,

    /// 0 - param, 1 - local
    pub env: Option<(Env, Env)>,
}

impl Expr {
    pub fn new(ty: Type, kind: ExprKind, errs: Vec<Error>) -> Self {
        Self {
            ty,
            kind,
            errs,
            env: None,
        }
    }

    pub fn new_ok(ty: Type, kind: ExprKind) -> Self {
        Self::new(ty, kind, vec![])
    }

    pub fn get_ty_from_syn(envs: &EnvStack, expr: &ya_syn::Expr) -> Result<Type, Error> {
        match expr {
            ya_syn::Expr::Lit(lit) => Ok(Type::from_lit(lit)?),
            ya_syn::Expr::Func(func) => Ok(Type::Func(func.into())),
            ya_syn::Expr::Symbol(symbol) => Ok(
                envs
                    .get_const(symbol.name.as_str())
                    .map(|c| c.rhs.ty.clone())
                    .or_else(|_| envs
                        .get_def_var(symbol.name.as_str())
                        .map(|ty| ty.clone()))?
            ),
            _ => Ok(Type::Prim(PrimType::Unit)),
        }
    }
}

impl ParseSynExpr for Expr {
    type SynExpr = ya_syn::Expr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        match expr {
            ya_syn::Expr::Const(expr) => ConstExpr::parse(envs, expr),
            ya_syn::Expr::Let(expr) => LetExpr::parse(envs, expr),
            ya_syn::Expr::Lit(expr) => LitExpr::parse(envs, expr),
            ya_syn::Expr::Symbol(expr) => SymbolExpr::parse(envs, expr),
            ya_syn::Expr::Block(expr) => BlockExpr::parse(envs, expr),
            ya_syn::Expr::Tuple(expr) => TupleExpr::parse(envs, expr),
            ya_syn::Expr::Call(expr) => CallExpr::parse(envs, expr),
            ya_syn::Expr::BinOp(expr) => BinOpExpr::parse(envs, expr),
            ya_syn::Expr::UnOp(expr) => UnOpExpr::parse(envs, expr),
            ya_syn::Expr::Func(expr) => FuncExpr::parse(envs, expr),
            ya_syn::Expr::Err(errs) => Expr::new(
                Type::Prim(PrimType::Unit),
                ExprKind::Block(BlockExpr { stmts: vec![], expr: None }),
                Vec::from_iter(errs.into_iter().map(|err| Error::Syntax(err.clone().into())))
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Const(ConstExpr),
    Let(LetExpr),
    Lit(LitExpr),
    Symbol(SymbolExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
    Call(CallExpr),
    BinOp(BinOpExpr),
    UnOp(UnOpExpr),
    Func(FuncExpr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConstExpr {
    pub symbol: String,
}

impl ConstExpr {
    pub fn parse_decl(envs: &mut EnvStack, expr: &ya_syn::ConstExpr) -> Expr {
        let expr = Self::parse(envs, expr);

        if let ExprKind::Const(Self { ref symbol }) = expr.kind {
            envs.stack
                .last_mut()
                .expect("No environment found").consts
                .insert(symbol.clone(), ConstInfo {
                    rhs: Expr::new_ok(expr.ty.clone(), ExprKind::Block(BlockExpr { stmts: vec![], expr: None })),
                    errs: expr.errs.clone(),
                });
        } else {
            unreachable!();
        }

        expr
    }
}

impl ParseSynExpr for ConstExpr {
    type SynExpr = ya_syn::ConstExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let symbol = expr.symbol.name.clone();
        let ty: Option<Type> = expr.ty.as_ref().map(|ty| ty.into());
        let deduced_ty = Expr::get_ty_from_syn(envs, &expr.expr)
            .unwrap_or_else(|err| {
                errs.push(err);
                Type::Prim(PrimType::Unit)
            });
        
        let ty = match (ty, deduced_ty) {
            (None, ty) => ty,
            (Some(l), r) if l == r => l,
            (Some(l), r) => {
                errs.push(Error::AssignmentMismatchedOperandTypes { lhs: l, rhs: r });
                Type::Prim(PrimType::Unit)
            }
        };

        Expr::new(
            ty,
            ExprKind::Const(Self {
                symbol,
            }),
            errs,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LetExpr {
    pub symbol: String,
}

impl ParseSynExpr for LetExpr {
    type SynExpr = ya_syn::LetExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let symbol = expr.symbol.name.clone();
        let ty = expr.ty.as_ref().map(|ty| ty.into());

        envs.stack
            .last_mut()
            .expect("No environment found").vars
            .insert(symbol.clone(), ty.clone());

        Expr::new_ok(
            ty.unwrap_or(Type::Prim(PrimType::Unit)),
            ExprKind::Let(Self {
                symbol,
            }),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum LitKind {
    String,
    Char,
    Integer,
    Float,
}

impl From<&ya_syn::token::LitKind> for LitKind {
    fn from(lit: &ya_syn::token::LitKind) -> Self {
        match lit {
            ya_syn::token::LitKind::String => LitKind::String,
            ya_syn::token::LitKind::Char => LitKind::Char,
            ya_syn::token::LitKind::Integer => LitKind::Integer,
            ya_syn::token::LitKind::Float => LitKind::Float,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LitExpr {
    pub value: String,
    pub prefix: String,
    pub suffix: String,
    pub kind: LitKind,
}

impl LitExpr {
    pub fn parse_without_env(expr: &ya_syn::token::Lit) -> Expr {
        let mut errs = vec![];

        let ty = match Type::from_lit(expr) {
            Ok(ty) => ty,
            Err(err) => {
                errs.push(err);
                Type::Prim(PrimType::Unit)
            },
        };

        Expr::new(
            ty,
            ExprKind::Lit(Self {
                value: expr.value.clone(),
                prefix: expr.prefix.clone(),
                suffix: expr.suffix.clone(),
                kind: (&expr.kind).into(),
            }),
            errs,
        )
    }
}

impl ParseSynExpr for LitExpr {
    type SynExpr = ya_syn::token::Lit;

    fn parse(_envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        LitExpr::parse_without_env(expr)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SymbolExpr {
    pub name: String,
}

impl ParseSynExpr for SymbolExpr {
    type SynExpr = ya_syn::token::Symbol;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let ty = envs.get_const(expr.name.as_str())
            .map(|c| &c.rhs.ty)
            .or_else(|_| envs.get_def_var(expr.name.as_str()))
            .map_err(|err| errs.push(err))
            .map_or(Type::Prim(PrimType::Unit), |ty| ty.clone());

        Expr::new(
            ty,
            ExprKind::Symbol(Self {
                name: expr.name.clone(),
            }),
            errs,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}

impl ParseSynExpr for BlockExpr {
    type SynExpr = ya_syn::BlockExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        // parse constant variable and types first
        let consts = expr.stmts
            .iter()
            .flat_map(|stmt| stmt.find_all_curr_scope(|expr| {
                // find constant definitions
                match expr {
                    ya_syn::Expr::Const(ya_syn::ConstExpr { .. }) => true,
                    _ => false,
                }
            }))
            .map(|expr| {
                match expr {
                    ya_syn::Expr::Const(expr) => {
                        ConstExpr::parse_decl(envs, expr);
                        expr
                    },
                    _ => unreachable!(),
                }
            })
            .collect::<Vec<_>>();
        
        for syn_expr in consts {
            envs.stack
                .last_mut()
                .expect("Cannot find environment")
                .get_const_mut(syn_expr.symbol.name.as_str())
                .expect("Cannot find constant")
                .rhs = Expr::parse(envs, &syn_expr.expr);
        }

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

        Expr::new_ok(
            ty,
            ExprKind::Block(Self {
                stmts,
                expr,
            }),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
}

impl ParseSynExpr for TupleExpr {
    type SynExpr = ya_syn::TupleExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let items = expr.items
            .iter()
            .map(|item| Expr::parse(envs, item))
            .collect::<Vec<_>>();

        let ty = Type::Tuple(items
            .iter()
            .map(|item| item.ty.clone())
            .collect());

        Expr::new_ok(
            ty,
            ExprKind::Tuple(Self {
                items,
            }),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

impl ParseSynExpr for CallExpr {
    type SynExpr = ya_syn::CallExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let args: Vec<Expr> = expr.args
            .iter()
            .map(|arg| Expr::parse(envs, arg))
            .collect();

        let callee = Box::new(Expr::parse(envs, &*expr.callee));

        // check arguments
        let ret_ty = match &callee.as_ref().ty {
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

                ty.ret_ty.as_ref().clone()
            },
            found => {
                errs.push(Error::ExpectedCallable { found: found.clone() });
                Type::Prim(PrimType::Unit)
            },
        };

        Expr::new(
            ret_ty,
            ExprKind::Call(CallExpr {
                callee,
                args
            }),
            errs,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
    ) -> BinOpInfo {
        let bin_op = BinOp {
            op: op.clone(),
            lhs: lhs.ty.clone(),
            rhs: rhs.ty.clone(),
        };

        envs.get_bin_op(&bin_op)
            .map_err(|err| errs.push(err))
            .unwrap_or(BinOpInfo::new_builtin(Type::Prim(PrimType::Unit), 0))
    }
}

impl ParseSynExpr for BinOpExpr {
    type SynExpr = ya_syn::BinOpExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        struct OpFlatInfo {
            info: BinOpInfo,
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
                let push_flat_info = |envs: &mut EnvStack, operands: &mut Vec<Option<Expr>>| {
                    // special case: $let_expr = $rhs
                    // initialize the var in $let_expr with $rhs
                    match (&mut operands[idx..idx + 2], curr_op.op.op.as_str()) {
                        ([
                            Some(lhs @ Expr {
                                kind: ExprKind::Let(_),
                                ..
                            }),
                            Some(rhs),
                        ], "=") => {
                            lhs.ty = rhs.ty.clone();

                            match &mut lhs.kind {
                                ExprKind::Let(ref mut let_expr) => {
                                    match envs.get_var_mut(let_expr.symbol.as_str()) {
                                        Ok(ty) => match ty {
                                            Some(ty) if rhs.ty == *ty => *ty = rhs.ty.clone(),
                                            Some(ty) => errs.push(Error::AssignmentMismatchedOperandTypes {
                                                lhs: ty.clone(),
                                                rhs: rhs.ty.clone()
                                            }),
                                            None => *ty = Some(rhs.ty.clone()),
                                        },
                                        Err(err) => errs.push(err),
                                    }
                                },
                                _ => {},
                            }

                            op_flat_infos.push(OpFlatInfo {
                                info: BinOpInfo::new_builtin(Type::Prim(PrimType::Unit), 0x0),
                                op: curr_op.op.op.clone(),
                                errs,
                                lhs: idx,
                                rhs: idx + 1,
                            });
                            return;
                        },
                        _ => {},
                    }
                    
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
                    ya_syn::Expr::BinOp(bin_expr) => {
                        operands.push(Some(Expr::parse(envs, &*bin_expr.lhs)));
                        push_flat_info(envs, &mut operands);

                        curr_op = bin_expr;
                        idx += 1;
                    },
                    _ => {
                        operands.push(Some(Expr::parse(envs, &*curr_op.rhs)));
                        push_flat_info(envs, &mut operands);
                        break;
                    },
                }
            }
        }

        fn resolve_prec(envs: &mut EnvStack, mut op_flat_infos: Vec<OpFlatInfo>, operands: &mut Vec<Option<Expr>>) -> Expr {
            if op_flat_infos.len() == 1 {
                let OpFlatInfo { 
                    info: BinOpInfo {
                        ty,
                        ..
                    },
                    op,
                    errs,
                    lhs,
                    rhs,
                } = op_flat_infos.pop().unwrap();

                return Expr::new(
                    ty,
                    ExprKind::BinOp(BinOpExpr {
                        op,
                        lhs: Box::new(std::mem::replace(&mut operands[lhs], None).unwrap()),
                        rhs: Box::new(std::mem::replace(&mut operands[rhs], None).unwrap()),
                    }),
                    errs,
                );
            }

            let root = op_flat_infos
                .iter()
                .enumerate()
                .max_by(|(_, a), (_, b)| b.info.prec.cmp(&a.info.prec))
                .unwrap();
            
            let root = if root.1.info.assoc == OpAssoc::Left {
                op_flat_infos
                    .iter()
                    .enumerate()
                    .rev()
                    .max_by(|(_, a), (_, b)| b.info.prec.cmp(&a.info.prec))
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

            Expr::new(
                BinOpExpr::get_op_info(envs, &mut root.errs, &root.op, &lhs, &rhs).ty.clone(),
                ExprKind::BinOp(BinOpExpr {
                    op: root.op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }),
                root.errs,
            )
        }

        resolve_prec(envs, op_flat_infos, &mut operands)
    }
}

impl From<ya_syn::UnOpPos> for UnOpPos {
    fn from(pos: ya_syn::UnOpPos) -> Self {
        if pos == ya_syn::UnOpPos::Pre { UnOpPos::Pre } else { UnOpPos::Suf }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnOpExpr {
    pub op: char,
    pub op_pos: UnOpPos,
    pub expr: Box<Expr>,
}

impl ParseSynExpr for UnOpExpr {
    type SynExpr = ya_syn::UnOpExpr;

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
        .map_or(Type::Prim(PrimType::Unit), |info| info.ty.clone());

        Expr::new(
            ty,
            ExprKind::UnOp(UnOpExpr {
                op,
                op_pos,
                expr,
            }),
            errs,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FuncExpr {
    pub id: usize,
}

impl ParseSynExpr for FuncExpr {
    type SynExpr = ya_syn::FuncExpr;

    fn parse(envs: &mut EnvStack, expr: &Self::SynExpr) -> Expr {
        let mut errs = vec![];

        let ty = Type::Func(expr.into());

        let vars = expr.params
            .iter()
            .map(|param| (param.name.name.clone(), Some((&param.ty).into())))
            .collect::<HashMap<_, _>>();
        
        let empty_block_expr = ya_syn::BlockExpr { stmts: vec![], expr: None, sep_errs: vec![] };
        let func = BlockExpr::parse_with_local_vars(
            envs,
            match expr.body.as_ref() {
                ya_syn::Expr::Block(expr) => expr,
                ya_syn::Expr::Err(err) => {
                    errs.extend(err.clone().into_iter().map(|err| Error::Syntax(err.into())));
                    &empty_block_expr
                },
                _ => unreachable!(),
            },
            vars,
        );
        envs.funcs.push(func);

        Expr::new(
            ty,
            ExprKind::Func(FuncExpr {
                id: envs.funcs.len() - 1,
            }),
            errs,
        )
    }
}