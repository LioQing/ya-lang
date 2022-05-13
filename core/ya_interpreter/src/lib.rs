use std::collections::HashMap;
use thiserror::Error;

use ya_core::*;
use ya_core::ya_sem::Type;
use ya_core::ya_prim_types::PrimType;

#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum Error {
    #[error("Unintialized variable {var}")]
    UninitVar { var: String },

    #[error("Moved variable {var}")]
    MovedVar { var: String },

    #[error("Var not found {var}")]
    VarNotFound { var: String },
}

macro_rules! bin_op_info {
    ($lhs:expr, $op:literal, $rhs:expr => $res:expr; $prec:literal) => {
        (
            ya_sem::BinOp { op: $op.to_owned(), lhs: $lhs, rhs: $rhs },
            ya_sem::BinOpInfo::new_builtin($res, $prec),
        )
    };
}

macro_rules! prim_bin_op_info {
    ($op:literal, $ty:ident => $res_ty:ident; $prec:literal) => {
        bin_op_info!(Type::Prim(PrimType::$ty), $op, Type::Prim(PrimType::$ty) => Type::Prim(PrimType::$res_ty); $prec)
    };
}

macro_rules! prim_arith_bin_op_info_vec {
    ($($ty:ident),+) => {
        [$(
            prim_bin_op_info!("+", $ty => $ty; 0xa),
            prim_bin_op_info!("-", $ty => $ty; 0xa),
            prim_bin_op_info!("*", $ty => $ty; 0xb),
            prim_bin_op_info!("/", $ty => $ty; 0xb),
            prim_bin_op_info!("%", $ty => $ty; 0xb),
        )+]
    };
}

macro_rules! un_op_info {
    ($op:literal, $ty:expr => $res:expr) => {
        (
            ya_sem::UnOp { op: $op, op_pos: ya_sem::UnOpPos::Pre, ty: $ty },
            ya_sem::UnOpInfo { ty: $res, func_id: None },
        )
    };
    ($ty:expr, $op:literal => $res:expr) => {
        (
            ya_sem::UnOp { op: $op, op_pos: ya_sem::UnOpPos::Suf, ty: $ty },
            ya_sem::UnOpInfo { ty: $res, func_id: None },
        )
    };
}

#[derive(Debug, PartialEq)]
enum VarVal {
    Some(ExprVal),
    Uninitialized,
    Moved,
}

#[derive(Debug, PartialEq)]
struct Env {
    pub vars: HashMap<String, VarVal>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct EnvStack {
    pub stack: Vec<Env>,
}

impl EnvStack {
    pub fn new() -> Self {
        Self {
            stack: vec![Env::new()],
        }
    }

    pub fn add_var(&mut self, var: String, val: VarVal) {
        self.stack.last_mut().unwrap().vars.insert(var, val);
    }

    pub fn assign_var(&mut self, var: &str, val: ExprVal) -> Result<(), Error> {
        let var_ref = self.stack.iter_mut().rev().find_map(|env| env.vars.get_mut(var));
        match var_ref {
            None => Err(Error::VarNotFound { var: var.to_owned() }),
            Some(VarVal::Moved) => Err(Error::MovedVar { var: var.to_owned() }),
            _ => {
                *var_ref.unwrap() = VarVal::Some(val);
                Ok(())
            }
        }
    }

    pub fn get_symbol_expr(&self, var: &str) -> Result<ExprVal, Error> {
        let var_ref = self.stack.iter().rev().find_map(|env| env.vars.get(var));
        match var_ref {
            None => Err(Error::VarNotFound { var: var.to_owned() }),
            Some(VarVal::Some(val)) => Ok(val.clone()),
            Some(VarVal::Uninitialized) => Err(Error::UninitVar { var: var.to_owned() }),
            Some(VarVal::Moved) => Err(Error::MovedVar { var: var.to_owned() }),
        }
    }
}

/// Evaluate the AST
pub fn run<P>(path: P) where P: AsRef<std::path::Path> {
    let bin_ops: HashMap<_, _> = prim_arith_bin_op_info_vec!(I8, I16, I32, I64, U8, U16, U32, U64, F32, F64).into();

    let un_ops: HashMap<_, _> = [
        un_op_info!('-', Type::Prim(PrimType::I32) => Type::Prim(PrimType::I32)),
    ].into();

    let src = std::fs::read_to_string(path).unwrap();

    let syn_parser = ya_syn::Parser::parse(&src);

    println!("{:#?}", syn_parser.items);

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

    println!("funcs: {:#?}", sem_parser.global_env.funcs);
    println!("items: {:#?}", sem_parser.items);

    let main = sem_parser.global_env.stack
        .first()
        .expect("Cannot find environment")
        .consts
        .iter()
        .find_map(|(var, info)| match var.as_str() {
            "main" => {
                match &info.rhs {
                    ya_sem::Expr { kind: ya_sem::ExprKind::Func(ya_sem::FuncExpr { id }), .. } => {
                        Some(&sem_parser.global_env.funcs[*id])
                    },
                    _ => None,
                }
            },
            _ => None,
        })
        .expect("Cannot find main function, the main functions has to be a constant function named main");
    
    let mut envs = EnvStack::new();

    match main {
        ya_sem::Expr { kind: ya_sem::ExprKind::Block(expr), .. } => {
            expr.stmts.iter().for_each(|stmt| {
                run_expr(&mut envs, stmt);
            });

            expr.expr.as_ref().map(|expr| {
                println!("main return: {:?}", run_expr(&mut envs, expr.as_ref()));
            });
        },
        _ => unreachable!(),
    }

    println!("{:#?}", envs.stack);
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ExprVal {
    Unit,
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(u8),
    ISize(isize),
    USize(usize),
}

macro_rules! try_err {
    ($e:expr) => {
        match $e {
            Ok(val) => val,
            Err(err) => {
                eprintln!("{}", err);
                return ExprVal::Unit;
            },
        }
    };
}

macro_rules! eval_arith_bin_op {
    ($envs:ident, $lhs:ident, $rhs:ident, $eval:tt; $($ty:ident),*) => {
        match ($lhs, $rhs) {
            $((ExprVal::$ty(l), ExprVal::$ty(r)) => ExprVal::$ty(l $eval r),)*
            _ => unimplemented!()
        }
    };
}

fn run_expr(envs: &mut EnvStack, expr: &ya_sem::Expr) -> ExprVal {
    if !expr.errs.is_empty() {
        for err in &expr.errs {
            eprintln!("{}", err);
        }

        return ExprVal::Unit;
    }

    match &expr.kind {
        ya_sem::ExprKind::Let(ya_sem::LetExpr { symbol }) => {
            envs.add_var(symbol.to_owned(), VarVal::Uninitialized);
            ExprVal::Unit
        },
        ya_sem::ExprKind::Lit(ya_sem::LitExpr { value, prefix, kind, .. }) => {
            match kind {
                ya_sem::LitKind::Integer | ya_sem::LitKind::Float => {
                    let radix = match prefix.as_str() {
                        "0x" => 16,
                        "0o" => 8,
                        "0b" => 2,
                        _ => 10,
                    };

                    match (&expr.ty, value) {
                        (Type::Prim(PrimType::I8), value)  => ExprVal::I8(i8::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::I16), value) => ExprVal::I16(i16::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::I32), value) => ExprVal::I32(i32::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::I64), value) => ExprVal::I64(i64::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::U8), value)  => ExprVal::U8(u8::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::U16), value) => ExprVal::U16(u16::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::U32), value) => ExprVal::U32(u32::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::U64), value) => ExprVal::U64(u64::from_str_radix(value, radix).unwrap()),
                        (Type::Prim(PrimType::F32), value) => ExprVal::F32(value.parse::<f32>().unwrap()),
                        (Type::Prim(PrimType::F64), value) => ExprVal::F64(value.parse::<f64>().unwrap()),
                        _ => unreachable!(),
                    }
                },
                _ => unimplemented!(),
            }
        },
        ya_sem::ExprKind::Symbol(ya_sem::SymbolExpr { name }) => {
            try_err!(envs.get_symbol_expr(name.as_str()))
        },
        ya_sem::ExprKind::BinOp(ya_sem::BinOpExpr { op, lhs: lhs_expr, rhs: rhs_expr }) => {
            let lhs = run_expr(envs, lhs_expr.as_ref());
            let rhs = run_expr(envs, rhs_expr.as_ref());

            match op.as_str() {
                // special case: "=" operator
                "=" => match &lhs_expr.as_ref().kind {
                    ya_sem::ExprKind::Let(let_expr) => {
                        try_err!(envs.assign_var(let_expr.symbol.as_str(), rhs));
                        ExprVal::Unit
                    },
                    _ => unimplemented!(),
                },
                "+" => eval_arith_bin_op!(envs, lhs, rhs, +; I8, I16, I32, I64, U8, U16, U32, U64, F32, F64),
                "-" => eval_arith_bin_op!(envs, lhs, rhs, -; I8, I16, I32, I64, U8, U16, U32, U64, F32, F64),
                "*" => eval_arith_bin_op!(envs, lhs, rhs, *; I8, I16, I32, I64, U8, U16, U32, U64, F32, F64),
                "/" => eval_arith_bin_op!(envs, lhs, rhs, /; I8, I16, I32, I64, U8, U16, U32, U64, F32, F64),
                "%" => eval_arith_bin_op!(envs, lhs, rhs, %; I8, I16, I32, I64, U8, U16, U32, U64, F32, F64),
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}