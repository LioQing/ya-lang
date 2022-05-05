use crate::syntax_parser as syn;
use crate::prim_type::PrimType;
use std::collections::HashMap;
use thiserror::Error;

#[cfg(test)]
mod tests;

pub mod expr;
pub mod env;

pub use expr::*;
pub use env::*;

#[derive(Error, Debug, PartialEq, Clone)]
pub enum Error {
    #[error("Global variable {var} not defined")]
    GlobalVarNotDefined { var: String },

    #[error("Invalid literal suffix {suffix}")]
    InvalidLiteralSuffix { suffix: String },

    #[error("Conflicting types {type1:?} and {type2:?}")]
    ConflictingTypes { type1: Type, type2: Type },

    #[error("Tuple missing type at index {index}")]
    TupleMissingType { index: usize },

    #[error("Expected callable, found {found:?}")]
    ExpectedCallable { found: Type },

    #[error("Mismatched argument type: expected {expected:?}, found {found:?}")]
    MismatchedArgument { expected: Option<Type>, found: Option<Type> },

    #[error("Assignment operation `=` with mismatched operand types {lhs:?} and {rhs:?}")]
    AssignmentMismatchedOperandTypes { lhs: Type, rhs: Type },

    #[error("Variable not found {var}")]
    VarNotFound { var: String },

    #[error("Type not found {ty}")]
    TypeNotFound { ty: String },

    #[error("Binary operator not found {lhs:?} {op} {rhs:?}")]
    BinOpNotFound { op: String, lhs: Type, rhs: Type },

    #[error("Prefix unary operator not found {op} {ty:?}")]
    PrefixUnOpNotFound { op: String, ty: Type },

    #[error("Suffix unary operator not found {ty:?} {op}")]
    SuffixUnOpNotFound { ty: Type, op: String },

    #[error("Undefined variable {var}")]
    UndefVar { var: String },
}

/// The semantic parser.
/// 
/// Performs semantic analysis.
/// Parse the parse tree into an AST.
pub struct Parser {
    pub global_env: Env,
    pub funcs: Vec<Expr>,
    pub errs: Vec<Error>,
}

impl Parser {
    pub fn parse(syn_items: &Vec<syn::Item>, env: Env) -> Self {
        let mut global = EnvStack {
            envs: vec![env],
        };
        let mut errs = vec![];

        // first scan: global items
        let mut funcs = vec![];
        for item in syn_items {
            match item {
                syn::Item::Def(expr) => {
                    let (name, ty) = match expr.lhs.as_ref() {
                        syn::Expr::Let(expr) => {
                            (
                                expr.var.name.clone(),
                                expr.ty
                                    .as_ref()
                                    .map(|ty| ty.into()),
                            )
                        },
                        _ => unimplemented!(),
                    };
                    
                    let deduced_ty = match Expr::get_ty_from_syn(&global, expr.rhs.as_ref())
                    {
                        Err(err) => {
                            errs.push(err);
                            continue;
                        },
                        ty => ty.ok(),
                    };

                    let ty = match (ty, deduced_ty) {
                        (Some(ty1), Some(ty2)) if ty1 != ty2 => {
                            errs.push(Error::ConflictingTypes { type1: ty1, type2: ty2 });
                            continue;
                        },
                        (_, Some(ty)) | (Some(ty), _) =>  Some(ty),
                        (None, None) => {
                            errs.push(Error::GlobalVarNotDefined { var: name });
                            continue;
                        },
                    };

                    // add function to be parsed later
                    if let (Some(Type::Func(_)), syn::Expr::Func(func)) = (&ty, expr.rhs.as_ref()) {
                        funcs.push(func.body.as_ref());
                    }

                    global.envs.first_mut().unwrap().vars.insert(name, ty);
                }
                syn::Item::Eof => {},
            }
        }

        // second scan: function body codes
        let funcs = funcs
            .iter()
            .map(|expr| BlockExpr::parse(&mut global, expr))
            .collect();

        Self {
            global_env: global.envs.pop().unwrap(),
            funcs,
            errs,
        }
    }
}