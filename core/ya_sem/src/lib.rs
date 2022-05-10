use ya_syn;
use ya_prim_types::PrimType;
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
    PrefixUnOpNotFound { op: char, ty: Type },

    #[error("Suffix unary operator not found {ty:?} {op}")]
    SuffixUnOpNotFound { ty: Type, op: char },

    #[error("Undefined variable {var}")]
    UndefVar { var: String },
}

/// The semantic parser.
/// 
/// Performs semantic analysis.
/// Parse the parse tree into an AST.
pub struct Parser {
    pub global_env: EnvStack,
    pub items: Vec<Expr>,
}

impl Parser {
    pub fn parse(syn_items: &Vec<ya_syn::Item>, env: Env) -> Self {
        let mut global = EnvStack {
            envs: vec![env],
            funcs: vec![],
        };
        let mut items = vec![];

        // first scan: global items
        for item in syn_items {
            match item {
                ya_syn::Item::Let(expr) => {
                    items.push(Expr::parse(&mut global, expr));
                }
                ya_syn::Item::Eof => {},
            }
        }

        Self {
            global_env: global,
            items,
        }
    }
}