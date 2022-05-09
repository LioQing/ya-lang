use super::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    pub tys: HashMap<String, Type>,
    pub vars: HashMap<String, Option<Type>>,
    pub bin_ops: HashMap<BinOp, OpInfo>,
    pub un_ops: HashMap<UnOp, Type>,
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

    pub fn get_var_mut(&mut self, var: &str) -> Result<&mut Option<Type>, Error> {
        self.vars
            .get_mut(var)
            .ok_or(Error::VarNotFound { var: var.to_owned() })
    }

    pub fn get_decl_var(&self, var: &str) -> Result<&Type, Error> {
        match self.get_var(var) {
            Ok(Some(ty)) => Ok(ty),
            Ok(None) => Err(Error::UndefVar { var: var.to_owned() }),
            Err(err) => Err(err),
        }
    }

    // Issue #2
    pub fn get_bin_op(&self, bin_op: &BinOp) -> Result<OpInfo, Error> {
        match bin_op.op.as_str() {
            // special case: "=" operator
            "=" => match bin_op.lhs.eq(&bin_op.rhs) {
                true => {
                    Ok(OpInfo::new(bin_op.lhs.clone(), 0x0))
                },
                false => {
                    Err(Error::AssignmentMismatchedOperandTypes {
                        lhs: bin_op.lhs.clone(),
                        rhs: bin_op.rhs.clone(),
                    })
                },
            },
            _ => {
                self.bin_ops
                .get(bin_op)
                .map(|op_info| op_info.clone())
                .ok_or(Error::BinOpNotFound {
                    op: bin_op.op.clone(),
                    lhs: bin_op.lhs.clone(),
                    rhs: bin_op.rhs.clone(),
                })
            },
        }
    }

    pub fn get_un_op(&self, un_op: &UnOp) -> Result<Type, Error> {
        self.un_ops
            .get(un_op)
            .map(|op_info| op_info.clone())
            .ok_or(match un_op.op_pos {
                UnOpPos::Pre => Error::PrefixUnOpNotFound {
                    op: un_op.op,
                    ty: un_op.ty.clone(),
                },
                UnOpPos::Suf => Error::SuffixUnOpNotFound {
                    ty: un_op.ty.clone(),
                    op: un_op.op,
                },
            })
    }
}

#[derive(Debug, Clone)]
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

    pub fn get_var_mut(&mut self, var: &str) -> Result<&mut Option<Type>, Error> {
        self.envs
            .iter_mut()
            .rev()
            .find_map(|env| match env.get_var_mut(var) {
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

    // Issue #2
    pub fn get_bin_op(&self, bin_op: &BinOp) -> Result<OpInfo, Error> {
        self.envs
            .iter()
            .rev()
            .find_map(|env| match env.get_bin_op(bin_op) {
                Ok(prec) => Some(prec),
                Err(_) => None,
            })
            .ok_or(Error::BinOpNotFound {
                op: bin_op.op.clone(),
                lhs: bin_op.lhs.clone(),
                rhs: bin_op.rhs.clone(),
            })
    }

    pub fn get_un_op(&self, un_op: &UnOp) -> Result<Type, Error> {
        self.envs
            .iter()
            .rev()
            .find_map(|env| match env.get_un_op(un_op) {
                Ok(prec) => Some(prec),
                Err(_) => None,
            })
            .ok_or(match un_op.op_pos {
                UnOpPos::Pre => Error::PrefixUnOpNotFound {
                    op: un_op.op,
                    ty: un_op.ty.clone(),
                },
                UnOpPos::Suf => Error::SuffixUnOpNotFound {
                    ty: un_op.ty.clone(),
                    op: un_op.op,
                },
            })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Prim(PrimType),
    Struct(Vec<Field>),
    Tuple(Vec<Type>),
    Func(FuncType),
}

impl Type {
    pub fn prim_type_from(lit: &ya_syn::token::Lit) -> Result<PrimType, Error> {
        match lit {
            ya_syn::token::Lit { suffix, kind: ya_syn::token::LitKind::Integer, .. } if suffix.is_empty() => {
                Ok(PrimType::I32)
            },
            ya_syn::token::Lit { suffix, kind: ya_syn::token::LitKind::Float, .. } if suffix.is_empty() => {
                Ok(PrimType::F32)
            },
            ya_syn::token::Lit {
                suffix,
                kind: kind @ (ya_syn::token::LitKind::Integer | ya_syn::token::LitKind::Float),
                ..
            } => {
                match (*kind, suffix.as_str()) {
                    (ya_syn::token::LitKind::Integer, "")    => Ok(PrimType::I32),
                    (ya_syn::token::LitKind::Float  , "")    => Ok(PrimType::F32),
                    (ya_syn::token::LitKind::Integer, "i8" ) => Ok(PrimType::I8 ),
                    (ya_syn::token::LitKind::Integer, "i16") => Ok(PrimType::I16),
                    (ya_syn::token::LitKind::Integer, "i32") => Ok(PrimType::I32),
                    (ya_syn::token::LitKind::Integer, "i64") => Ok(PrimType::I64),
                    (ya_syn::token::LitKind::Integer, "u8" ) => Ok(PrimType::U8 ),
                    (ya_syn::token::LitKind::Integer, "u16") => Ok(PrimType::U16),
                    (ya_syn::token::LitKind::Integer, "u32") => Ok(PrimType::U32),
                    (ya_syn::token::LitKind::Integer, "u64") => Ok(PrimType::U64),
                    (ya_syn::token::LitKind::Integer | ya_syn::token::LitKind::Float, "f32") => Ok(PrimType::F32),
                    (ya_syn::token::LitKind::Integer | ya_syn::token::LitKind::Float, "f64") => Ok(PrimType::F64),
                    _ => Err(Error::InvalidLiteralSuffix { suffix: suffix.clone() }),
                }
            },
            ya_syn::token::Lit { kind: ya_syn::token::LitKind::Char, .. } => {
                Ok(PrimType::Char)
            },
            _ => {
                unimplemented!()
            },
        }
    }
}

impl From<&ya_syn::token::TypeName> for Type {
    fn from(ty: &ya_syn::token::TypeName) -> Self {
        match ty {
            ya_syn::token::TypeName::PrimType(prim_type) => Type::Prim(*prim_type),
            ya_syn::token::TypeName::Struct(_) => Type::Struct(vec![]),
            ya_syn::token::TypeName::Tuple(tys) => Type::Tuple(tys.iter().map(|ty| ty.into()).collect()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub ret_ty: Box<Type>,
}

impl From<&ya_syn::FuncExpr> for FuncType {
    fn from(expr: &ya_syn::FuncExpr) -> Self {
        FuncType {
            params: expr.params.iter().map(|p| Type::from(&p.ty)).collect(),
            ret_ty: Box::new(Type::from(&expr.ret_ty)),
        }
    }
}

#[allow(unused_macros)]
macro_rules! bin_op_info {
    ($lhs:expr, $op:literal, $rhs:expr => $res:expr; $prec:literal, $assoc:ident) => {
        (
            semantic_parser::BinOp { op: $op.to_owned(), lhs: $lhs, rhs: $rhs },
            semantic_parser::OpInfo::new($res, $prec),
        )
    };
}

pub(super) use bin_op_info;

#[allow(unused_macros)]
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

pub(super) use un_op_info;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct BinOp {
    pub op: String,
    pub lhs: Type,
    pub rhs: Type,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum UnOpPos {
    Pre,
    Suf,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct UnOp {
    pub op: char,
    pub op_pos: UnOpPos,
    pub ty: Type,
}

pub type OpPrec = u8;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum OpAssoc {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OpInfo {
    pub ty: Type,
    pub prec: OpPrec,
    pub assoc: OpAssoc,
    pub func_id: Option<usize>,
}

impl OpInfo {
    pub fn new(ty: Type, prec: OpPrec) -> Self {
        Self {
            ty,
            prec,
            assoc: [OpAssoc::Left, OpAssoc::Right][(prec as usize / 2) % 2],
            func_id: None,
        }
    }
}