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

#[derive(Error, Debug, PartialEq, Eq, Clone, Hash)]
pub enum Error {
    #[error("{0}")]
    Syntax(#[from] ya_syn::Error),

    #[error("Global symbol {symbol} not defined")]
    GlobalVarNotDefined { symbol: String },

    #[error("Invalid literal suffix {suffix}")]
    InvalidLiteralSuffix { suffix: String },

    #[error("Radix prefix not supported for floating-point numeric literals")]
    RadixPrefixNotSupportedForFloat,

    #[error("Conflicting types {type1:?} and {type2:?}")]
    ConflictingTypes { type1: Type, type2: Type },

    #[error("Tuple missing type at index {index}")]
    TupleMissingType { index: usize },

    #[error("Expected callable, found {found:?}")]
    ExpectedCallable { found: Type },

    #[error("Expected keyword `{expected}`, found `{found}`")]
    ExpectedKeyword { expected: String, found: String },

    #[error("Mismatched argument type: expected {expected:?}, found {found:?}")]
    MismatchedArgument { expected: Option<Type>, found: Option<Type> },

    #[error("Assignment operation `=` with mismatched operand types {lhs:?} and {rhs:?}")]
    AssignmentMismatchedOperandTypes { lhs: Type, rhs: Type },

    #[error("Variable not found {var}")]
    VarNotFound { var: String },

    #[error("Constant not found {c}")]
    ConstNotFound { c: String },

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

    #[error("Expected global item `const`, found {found}")]
    ExpectedGlobalItem { found: String },
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
            stack: vec![env],
            funcs: vec![],
        };
        let mut items = vec![];

        // first scan: parse declarations
        let item_decls = syn_items
            .iter()
            .map(|item| match item {
                    ya_syn::Item::Const(ya_syn::Expr::Const(expr @ ya_syn::ConstExpr {
                        expr: rhs,
                        ..
                    })) => {
                        Some((ConstExpr::parse_decl(&mut global, expr), &**rhs))
                    },
                    _ => None,
                }
            )
            .collect::<Vec<_>>();

        // second scan: parse definitions
        for item in item_decls {
            match item {
                Some((Expr {
                    ty,
                    kind: ExprKind::Const(const_expr),
                    errs,
                    env,
                }, rhs))  => {
                    global.stack
                        .last_mut()
                        .expect("Cannot find environment")
                        .get_const_mut(const_expr.symbol.as_str())
                        .expect("Cannot find constant")
                        .rhs = Expr::parse(&mut global, rhs);
                    items.push(Expr { ty, kind: ExprKind::Const(const_expr), errs, env });
                },
                None => {},
                _ => unimplemented!(),
            }
        }

        Self {
            global_env: global,
            items,
        }
    }
}