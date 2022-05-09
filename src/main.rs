extern crate ya_core;
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

fn main() {
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

    let src = std::fs::read_to_string("core/examples/hello_world.ya").unwrap();

    let syn_parser = ya_syn::Parser::parse(&src);

    println!("{:#?}", syn_parser.items);
    println!("{:#?}", syn_parser.errs);

    let sem_parser = ya_sem::Parser::parse(
        &syn_parser.items,
        ya_sem::Env {
            tys: HashMap::new(),
            vars: HashMap::new(),
            bin_ops,
            un_ops,
        }
    );

    println!("env: {:#?}", sem_parser.global_env);
    println!("funcs: {:#?}", sem_parser.funcs);
    println!("errs: {:#?}", sem_parser.errs);
}