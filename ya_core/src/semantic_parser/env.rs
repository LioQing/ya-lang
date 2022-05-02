use super::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    pub tys: HashMap<String, Type>,
    pub vars: HashMap<String, Option<Type>>,
}

impl Env {
    pub fn get_ty(&self, ty: &str) -> Result<&Type, Error> {
        self.tys
            .get(ty)
            .ok_or(Error::TypeNotFound { ty: ty.to_owned() })
    }

    pub fn get_var(&self, var: &str) -> Result<&Option<Type>, Error> {
        self.vars
            .get(var)
            .ok_or(Error::VarNotFound { var: var.to_owned() })
    }

    pub fn get_decl_var(&self, var: &str) -> Result<&Type, Error> {
        match self.get_var(var) {
            Ok(Some(ty)) => Ok(ty),
            Ok(None) => Err(Error::UndefVar { var: var.to_owned() }),
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvStack {
    pub envs: Vec<Env>,
}

impl EnvStack {
    pub fn get_ty(&self, ty: &str) -> Result<&Type, Error> {
        self.envs
            .iter()
            .rev()
            .find_map(|env| match env.get_ty(ty) {
                Ok(ty) => Some(ty),
                Err(_) => None,
            })
            .ok_or(Error::TypeNotFound { ty: ty.to_owned() })
    }

    pub fn get_var(&self, var: &str) -> Result<&Option<Type>, Error> {
        self.envs
            .iter()
            .rev()
            .find_map(|env| match env.get_var(var) {
                Ok(ty) => Some(ty),
                Err(_) => None,
            })
            .ok_or(Error::VarNotFound { var: var.to_owned() })
    }

    pub fn get_def_var(&self, var: &str) -> Result<&Type, Error> {
        match self.get_var(var) {
            Ok(Some(ty)) => Ok(ty),
            Ok(None) => Err(Error::UndefVar { var: var.to_owned() }),
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    PrimType(PrimType),
    Struct(HashMap<String, Type>),
    Tuple(Vec<Type>),
    Func(FuncType),
}

impl From<&syn::token::TypeName> for Type {
    fn from(ty: &syn::token::TypeName) -> Self {
        match ty {
            syn::token::TypeName::PrimType(prim_type) => Type::PrimType(*prim_type),
            syn::token::TypeName::Struct(_) => Type::Struct(HashMap::new()),
            syn::token::TypeName::Tuple(tys) => Type::Tuple(tys.iter().map(|ty| ty.into()).collect()),
        }
    }
}

impl From<&syn::token::Lit> for Result<PrimType, Error> {
    fn from(lit: &syn::token::Lit) -> Self {
        match lit {
            syn::token::Lit { suffix, kind: syn::token::LitKind::Integer, .. } if suffix.is_empty() => {
                Ok(PrimType::I32)
            },
            syn::token::Lit { suffix, kind: syn::token::LitKind::Float, .. } if suffix.is_empty() => {
                Ok(PrimType::F32)
            },
            syn::token::Lit {
                suffix,
                kind: kind @ (syn::token::LitKind::Integer | syn::token::LitKind::Float),
                ..
            } => {
                match (*kind, suffix.as_str()) {
                    (syn::token::LitKind::Integer, "" )   => Ok(PrimType::I32),
                    (syn::token::LitKind::Float  , "" )   => Ok(PrimType::F32),
                    (syn::token::LitKind::Integer, "i8" ) => Ok(PrimType::I8 ),
                    (syn::token::LitKind::Integer, "i16") => Ok(PrimType::I16),
                    (syn::token::LitKind::Integer, "i32") => Ok(PrimType::I32),
                    (syn::token::LitKind::Integer, "i64") => Ok(PrimType::I64),
                    (syn::token::LitKind::Integer, "u8" ) => Ok(PrimType::U8 ),
                    (syn::token::LitKind::Integer, "u16") => Ok(PrimType::U16),
                    (syn::token::LitKind::Integer, "u32") => Ok(PrimType::U32),
                    (syn::token::LitKind::Integer, "u64") => Ok(PrimType::U64),
                    (syn::token::LitKind::Integer | syn::token::LitKind::Float, "f32") => Ok(PrimType::F32),
                    (syn::token::LitKind::Integer | syn::token::LitKind::Float, "f64") => Ok(PrimType::F64),
                    _ => Err(Error::InvalidLiteralSuffix { suffix: suffix.clone() }),
                }
            },
            syn::token::Lit { kind: syn::token::LitKind::Char, .. } => {
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

impl From<&syn::FuncExpr> for FuncType {
    fn from(expr: &syn::FuncExpr) -> Self {
        FuncType {
            params: expr.params.iter().map(|p| Type::from(&p.ty)).collect(),
            ret: Box::new(Type::from(&expr.ret_ty)),
        }
    }
}