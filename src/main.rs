extern crate ya_core;
use std::collections::HashMap;

use ya_core::*;
use ya_core::semantic_parser::Type;

macro_rules! bin_op_info {
    ($lhs:expr, $op:literal, $rhs:expr => $res:expr; $prec:literal, $assoc:ident) => {
        (
            semantic_parser::BinOp { op: $op.to_owned(), lhs: $lhs, rhs: $rhs },
            semantic_parser::OpInfo { ty: $res, prec: $prec, assoc: semantic_parser::OpAssoc::$assoc },
        )
    };
}

fn main() {
    let src = std::fs::read_to_string("ya_core/examples/hello_world.ya").unwrap();

    let synt_parser = syntax_parser::Parser::parse(&src);

    println!("{:#?}", synt_parser.items);
    println!("{:#?}", synt_parser.errs);

    let sem_parser = semantic_parser::Parser::parse(
        &synt_parser.items,
        semantic_parser::Env {
            tys: HashMap::new(),
            vars: HashMap::new(),
            bin_ops: [ // TODO: add built-in operators
                bin_op_info!(
                    Type::Prim(PrimType::I32), "+", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32);
                    0x5, Ltr
                ),
                bin_op_info!(
                    Type::Prim(PrimType::I32), "*", Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32);
                    0x4, Ltr
                ),
            ].into(),
            un_ops: [
                (
                    semantic_parser::UnOp { op: "!".to_owned(), op_pos: semantic_parser::UnOpPos::Pre, ty: Type::Prim(PrimType::I32) },
                    semantic_parser::Type::Prim(PrimType::I32),
                ),
            ].into(),
        }
    );

    println!("env: {:#?}", sem_parser.global_env);
    println!("funcs: {:#?}", sem_parser.funcs);
    println!("errs: {:#?}", sem_parser.errs);
}