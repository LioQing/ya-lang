use crate::syntax_parser as synt;
use crate::prim_type::PrimType;
use thiserror::Error;

pub mod item;

pub use item::*;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("Global variable {var} not defined")]
    GlobalVarNotDefined { var: String },

    #[error("Invalid literal suffix {suffix}")]
    InvalidLiteralSuffix { suffix: String },
}

/// The semantic parser.
/// 
/// Performs semantic analysis.
/// Parse the parse tree into an AST.
pub struct Parser {
    pub global: Env,
    pub errs: Vec<Error>,
}

impl Parser {
    pub fn parse(syntax_items: &Vec<synt::Item>) -> Self {
        let mut global = Env {
            ty: vec![],
            vars: vec![],
        };
        let mut errs = vec![];

        for item in syntax_items {
            match item {
                synt::Item::Let(expr) => {
                    let name = expr.var.name.clone();

                    let ty = expr.ty
                        .as_ref()
                        .map(|ty| ty.into());
                    
                    let deduced_ty = match expr.expr
                        .as_ref()
                        .map_or(Ok(Type::PrimType(PrimType::Unit)), |expr| {
                            Expr::get_ty_from_syntax(expr.as_ref())
                        })
                    {
                        Ok(ty) => ty,
                        Err(err) => {
                            errs.push(err);
                            continue;
                        }
                    };
                    
                    if matches!(&ty, Some(ty) if *ty != deduced_ty) {
                        errs.push(Error::GlobalVarNotDefined { var: name });
                        continue;
                    }

                    global.vars.push(Var {
                        ty: ty.map_or(Some(deduced_ty), |ty| Some(ty)),
                        name
                    })
                }
                synt::Item::Eof => {},
            }
        }

        Self {
            global,
            errs,
        }
    }
}