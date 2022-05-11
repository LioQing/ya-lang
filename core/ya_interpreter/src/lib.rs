use std::collections::HashMap;

use ya_core::*;
use ya_core::ya_sem::Type;
use ya_core::ya_prim_types::PrimType;

macro_rules! bin_op_info {
    ($lhs:expr, $op:literal, $rhs:expr => $res:expr; $prec:literal) => {
        (
            ya_sem::BinOp { op: $op.to_owned(), lhs: $lhs, rhs: $rhs },
            ya_sem::OpInfo::new($res, $prec),
        )
    };
}

macro_rules! un_op_info {
    ($op:literal, $ty:expr => $res:expr) => {
        (
            ya_sem::UnOp { op: $op, op_pos: ya_sem::UnOpPos::Pre, ty: $ty },
            $res,
        )
    };
    ($ty:expr, $op:literal => $res:expr) => {
        (
            ya_sem::UnOp { op: $op, op_pos: ya_sem::UnOpPos::Suf, ty: $ty },
            $res,
        )
    };
}

pub fn run() {
    let bin_ops: HashMap<_, _> = [
        bin_op_info!(Type::Prim(PrimType::I32), "==", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x9),
        bin_op_info!(Type::Prim(PrimType::I32), "!=", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x9),
        bin_op_info!(Type::Prim(PrimType::I32), "<", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x8),
        bin_op_info!(Type::Prim(PrimType::I32), "<=", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x8),
        bin_op_info!(Type::Prim(PrimType::I32), ">", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x8),
        bin_op_info!(Type::Prim(PrimType::I32), ">=", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x8),
        bin_op_info!(Type::Prim(PrimType::I32), "+", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x5),
        bin_op_info!(Type::Prim(PrimType::I32), "-", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x5),
        bin_op_info!(Type::Prim(PrimType::I32), "*", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x4),
        bin_op_info!(Type::Prim(PrimType::I32), "/", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x4),
        bin_op_info!(Type::Prim(PrimType::I32), "%", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32); 0x4),
    ].into();

    let un_ops: HashMap<_, _> = [
        un_op_info!('-', Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32)),
    ].into();

    let src = std::fs::read_to_string("./examples/hello_world.ya").unwrap();

    let syn_parser = ya_syn::Parser::parse(&src);

    if !syn_parser.errs.is_empty() {
        println!("{:#?}", syn_parser.errs);
        return;
    }

    let sem_parser = ya_sem::Parser::parse(
        &syn_parser.items,
        ya_sem::Env {
            tys: HashMap::new(),
            vars: HashMap::new(),
            bin_ops,
            un_ops,
            consts: HashMap::new(),
        }
    );

    println!("env: {:#?}", sem_parser.global_env);
    println!("items: {:#?}", sem_parser.items);

    let main = sem_parser.global_env.envs
        .first()
        .expect("Cannot find environment")
        .consts
        .iter()
        .find_map(|(var, info)| match var.as_str() {
            "main" => {
                match &info.expr {
                    ya_sem::Expr { kind: ya_sem::ExprKind::Func(ya_sem::FuncExpr { id }), .. } => {
                        Some(&sem_parser.global_env.funcs[*id])
                    },
                    _ => None,
                }
            },
            _ => None,
        })
        .expect("Cannot find main function");
    
    match main {
        ya_sem::Expr { kind: ya_sem::ExprKind::Block(expr), .. } => {
            expr.expr.as_ref().map(|expr| {
                println!("main return: {:?}", run_expr(expr.as_ref()));
            });
        },
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ExprVal {
    I32(i32),
}

fn run_expr(expr: &ya_sem::Expr) -> ExprVal {
    match &expr.kind {
        ya_sem::ExprKind::Lit(ya_sem::LitExpr { value, kind, .. }) => {
            match kind {
                ya_sem::LitKind::Integer => ExprVal::I32(value.parse().expect("failed to parse int literal")),
                _ => unimplemented!(),
            }
        },
        ya_sem::ExprKind::BinOp(ya_sem::BinOpExpr { op, lhs, rhs }) => {
            match op.as_str() {
                "+" => match (run_expr(lhs.as_ref()), run_expr(rhs.as_ref())) {
                    (ExprVal::I32(l), ExprVal::I32(r)) => ExprVal::I32(l + r),
                },
                "-" => match (run_expr(lhs.as_ref()), run_expr(rhs.as_ref())) {
                    (ExprVal::I32(l), ExprVal::I32(r)) => ExprVal::I32(l - r),
                },
                "*" => match (run_expr(lhs.as_ref()), run_expr(rhs.as_ref())) {
                    (ExprVal::I32(l), ExprVal::I32(r)) => ExprVal::I32(l * r),
                },
                "/" => match (run_expr(lhs.as_ref()), run_expr(rhs.as_ref())) {
                    (ExprVal::I32(l), ExprVal::I32(r)) => ExprVal::I32(l / r),
                },
                "%" => match (run_expr(lhs.as_ref()), run_expr(rhs.as_ref())) {
                    (ExprVal::I32(l), ExprVal::I32(r)) => ExprVal::I32(l % r),
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}