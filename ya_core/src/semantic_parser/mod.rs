use crate::syntax_parser as syn;
use crate::prim_type::PrimType;
use thiserror::Error;

pub mod expr;
pub mod env;

pub use expr::*;
pub use env::*;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("Global variable {var} not defined")]
    GlobalVarNotDefined { var: String },

    #[error("Invalid literal suffix {suffix}")]
    InvalidLiteralSuffix { suffix: String },

    #[error("Conflicting types {type1:?} and {type2:?}")]
    ConflictingTypes { type1: Type, type2: Type },
}

/// The semantic parser.
/// 
/// Performs semantic analysis.
/// Parse the parse tree into an AST.
pub struct Parser {
    pub global_env: Env,
    pub funcs: Vec<FuncExpr>,
    pub errs: Vec<Error>,
}

impl Parser {
    pub fn parse(syn_items: &Vec<syn::Item>) -> Self {
        let mut global = Env {
            ty: vec![],
            vars: vec![],
        };
        let mut errs = vec![];

        // first scan: global items
        let mut funcs = vec![];
        for item in syn_items {
            match item {
                syn::Item::Let(expr) => {
                    let name = expr.var.name.clone();
                    
                    let deduced_ty = match expr.expr
                        .as_ref()
                        .map(|expr| {
                            Expr::get_ty_from_syn(expr.as_ref())
                        })
                    {
                        Some(Err(err)) => {
                            errs.push(err);
                            continue;
                        },
                        ty => ty.map(|r| r.unwrap()),
                    };
                    
                    let ty = expr.ty
                        .as_ref()
                        .map(|ty| ty.into());

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
                    if matches!((&ty, &expr.expr), (Some(Type::Func(_)), Some(_))) {
                        funcs.push(expr.expr.as_ref().unwrap());
                    }

                    global.vars.push(Var {
                        ty,
                        name,
                    })
                }
                syn::Item::Eof => {},
            }
        }

        // // second scan: function codes
        // let funcs = for expr in funcs {

        // };

        Self {
            global_env: global,
            funcs: vec![], // TODO: implement function body parsing
            errs,
        }
    }
}