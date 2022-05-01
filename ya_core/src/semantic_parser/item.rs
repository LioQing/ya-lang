use super::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Let(LetExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
}

impl Expr {
    pub fn get_ty_from_syntax(expr: &synt::Expr) -> Result<Type, Error> {
        match expr {
            synt::Expr::Let(_) => Ok(Type::PrimType(PrimType::Unit)),
            synt::Expr::Func(ref func) => Ok(Type::Func(func.into())),
            synt::Expr::Lit(ref lit) => Ok(Type::PrimType(<Result<PrimType, Error>>::from(lit)?)),
            _ => Ok(Type::PrimType(PrimType::Unit)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Let(LetExpr),
    Func(FuncExpr),
    Block(BlockExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    pub ty: Vec<Type>,
    pub vars: Vec<Var>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub ty: Option<Type>,
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    PrimType(PrimType),
    Struct(String),
    Tuple(Vec<Type>),
    Func(FuncType),
}

impl From<&synt::token::TypeName> for Type {
    fn from(ty: &synt::token::TypeName) -> Self {
        match ty {
            synt::token::TypeName::PrimType(prim_type) => Type::PrimType(*prim_type),
            synt::token::TypeName::Struct(name) => Type::Struct(name.clone()),
            synt::token::TypeName::Tuple(tys) => Type::Tuple(tys.iter().map(|ty| ty.into()).collect()),
        }
    }
}

impl From<&synt::token::Lit> for Result<PrimType, Error> {
    fn from(lit: &synt::token::Lit) -> Self {
        match lit {
            synt::token::Lit { suffix, kind: synt::token::LitKind::Integer, .. } if suffix.is_empty() => {
                Ok(PrimType::I32)
            },
            synt::token::Lit { suffix, kind: synt::token::LitKind::Float, .. } if suffix.is_empty() => {
                Ok(PrimType::F32)
            },
            synt::token::Lit {
                suffix,
                kind: kind @ (synt::token::LitKind::Integer | synt::token::LitKind::Float),
                ..
            } => {
                match (*kind, suffix.as_str()) {
                    (synt::token::LitKind::Integer, "" )   => Ok(PrimType::I32),
                    (synt::token::LitKind::Float  , "" )   => Ok(PrimType::F32),
                    (synt::token::LitKind::Integer, "i8" ) => Ok(PrimType::I8 ),
                    (synt::token::LitKind::Integer, "i16") => Ok(PrimType::I16),
                    (synt::token::LitKind::Integer, "i32") => Ok(PrimType::I32),
                    (synt::token::LitKind::Integer, "i64") => Ok(PrimType::I64),
                    (synt::token::LitKind::Integer, "u8" ) => Ok(PrimType::U8 ),
                    (synt::token::LitKind::Integer, "u16") => Ok(PrimType::U16),
                    (synt::token::LitKind::Integer, "u32") => Ok(PrimType::U32),
                    (synt::token::LitKind::Integer, "u64") => Ok(PrimType::U64),
                    (synt::token::LitKind::Integer | synt::token::LitKind::Float, "f32") => Ok(PrimType::F32),
                    (synt::token::LitKind::Integer | synt::token::LitKind::Float, "f64") => Ok(PrimType::F64),
                    _ => Err(Error::InvalidLiteralSuffix { suffix: suffix.clone() }),
                }
            },
            synt::token::Lit { kind: synt::token::LitKind::Char, .. } => {
                Ok(PrimType::Char)
            },
            _ => {
                unimplemented!()
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

impl From<&synt::FuncExpr> for FuncType {
    fn from(expr: &synt::FuncExpr) -> Self {
        FuncType {
            params: expr.params.iter().map(|p| Type::from(&p.ty)).collect(),
            ret: Box::new(Type::from(&expr.ret_ty)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub var: Var,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncExpr {
    pub ty: FuncType,
    pub body: Box<BlockExpr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockExpr {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}