extern crate ya_core;
use std::collections::HashMap;

use ya_core::*;
use ya_core::semantic_parser::Type;

macro_rules! bin_op_info {
    ($lhs:expr, $op:literal, $rhs:expr => $res:expr; $prec:literal) => {
        (
            semantic_parser::BinOp { op: $op.to_owned(), lhs: $lhs, rhs: $rhs },
            semantic_parser::OpInfo::new($res, $prec),
        )
    };
}

macro_rules! un_op_info {
    ($op:literal, $ty:expr => $res:expr) => {
        (
            semantic_parser::UnOp { op: $op, op_pos: semantic_parser::UnOpPos::Pre, ty: $ty },
            $res,
        )
    };
    ($ty:expr, $op:literal => $res:expr) => {
        (
            semantic_parser::UnOp { op: $op, op_pos: semantic_parser::UnOpPos::Suf, ty: $ty },
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

    let src = std::fs::read_to_string("ya_core/examples/hello_world.ya").unwrap();

    let syn_parser = syntax_parser::Parser::parse(&src);

    println!("{:#?}", syn_parser.items);
    println!("{:#?}", syn_parser.errs);

    let sem_parser = semantic_parser::Parser::parse(
        &syn_parser.items,
        semantic_parser::Env {
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