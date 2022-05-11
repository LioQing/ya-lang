use super::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Env {
    pub tys: HashMap<String, Type>,
    pub vars: HashMap<String, Option<Type>>,
    pub bin_ops: HashMap<BinOp, OpInfo>,
    pub un_ops: HashMap<UnOp, Type>,
    pub consts: HashMap<String, ConstInfo>,
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

    pub fn get_def_var(&self, var: &str) -> Result<&Type, Error> {
        match self.get_var(var) {
            Ok(Some(ty)) => Ok(ty),
            Ok(None) => Err(Error::UndefVar { var: var.to_owned() }),
            Err(err) => Err(err),
        }
    }

    pub fn get_const(&self, c: &str) -> Result<&ConstInfo, Error> {
        self.consts
            .get(c)
            .ok_or(Error::ConstNotFound { c: c.to_owned() })
    }

    pub fn get_const_mut(&mut self, c: &str) -> Result<&mut ConstInfo, Error> {
        self.consts
            .get_mut(c)
            .ok_or(Error::ConstNotFound { c: c.to_owned() })
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

#[derive(Debug, PartialEq, Eq)]
pub struct EnvStack {
    pub envs: Vec<Env>,
    pub funcs: Vec<Expr>,
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

    pub fn get_const(&self, c: &str) -> Result<&ConstInfo, Error> {
        self.envs
            .iter()
            .rev()
            .find_map(|env| match env.get_const(c) {
                Ok(c) => Some(c),
                Err(_) => None,
            })
            .ok_or(Error::ConstNotFound { c: c.to_owned() })
    }

    pub fn get_const_mut(&mut self, c: &str) -> Result<&mut ConstInfo, Error> {
        self.envs
            .iter_mut()
            .rev()
            .find_map(|env| match env.get_const_mut(c) {
                Ok(c) => Some(c),
                Err(_) => None,
            })
            .ok_or(Error::ConstNotFound { c: c.to_owned() })
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
    Array(ArrayType),
    Func(FuncType),
}

impl Type {
    pub fn from_lit(lit: &ya_syn::token::Lit) -> Result<Type, Error> {
        match lit {
            ya_syn::token::Lit { suffix, kind: ya_syn::token::LitKind::Integer, .. } if suffix.is_empty() => {
                Ok(Self::Prim(PrimType::I32))
            },
            ya_syn::token::Lit { suffix, kind: ya_syn::token::LitKind::Float, .. } if suffix.is_empty() => {
                Ok(Self::Prim(PrimType::F32))
            },
            ya_syn::token::Lit {
                suffix,
                kind: kind @ (ya_syn::token::LitKind::Integer | ya_syn::token::LitKind::Float),
                ..
            } => {
                match (*kind, suffix.as_str()) {
                    (ya_syn::token::LitKind::Integer, "")    => Ok(Self::Prim(PrimType::I32)),
                    (ya_syn::token::LitKind::Float  , "")    => Ok(Self::Prim(PrimType::F32)),
                    (ya_syn::token::LitKind::Integer, "i8" ) => Ok(Self::Prim(PrimType::I8 )),
                    (ya_syn::token::LitKind::Integer, "i16") => Ok(Self::Prim(PrimType::I16)),
                    (ya_syn::token::LitKind::Integer, "i32") => Ok(Self::Prim(PrimType::I32)),
                    (ya_syn::token::LitKind::Integer, "i64") => Ok(Self::Prim(PrimType::I64)),
                    (ya_syn::token::LitKind::Integer, "u8" ) => Ok(Self::Prim(PrimType::U8 )),
                    (ya_syn::token::LitKind::Integer, "u16") => Ok(Self::Prim(PrimType::U16)),
                    (ya_syn::token::LitKind::Integer, "u32") => Ok(Self::Prim(PrimType::U32)),
                    (ya_syn::token::LitKind::Integer, "u64") => Ok(Self::Prim(PrimType::U64)),
                    (ya_syn::token::LitKind::Integer | ya_syn::token::LitKind::Float, "f32") => Ok(Self::Prim(PrimType::F32)),
                    (ya_syn::token::LitKind::Integer | ya_syn::token::LitKind::Float, "f64") => Ok(Self::Prim(PrimType::F64)),
                    _ => Err(Error::InvalidLiteralSuffix { suffix: suffix.clone() }),
                }
            },
            ya_syn::token::Lit { kind: ya_syn::token::LitKind::Char, .. } => {
                Ok(Self::Prim(PrimType::Char))
            },
            ya_syn::token::Lit {
                value,
                prefix,
                suffix,
                kind: ya_syn::token::LitKind::String,
            } if prefix.is_empty() && suffix.is_empty() && value.is_ascii() => {
                Ok(Self::Array(ArrayType {
                    ty: Box::new(Self::Prim(PrimType::Char)),
                    len: LitExpr {
                        value: value.clone(),
                        prefix: prefix.clone(),
                        suffix: suffix.clone(),
                        kind: LitKind::String,
                    },
                }))
            },
            _ => unimplemented!(),
        }
    }
}

impl From<&ya_syn::token::TypeName> for Type {
    fn from(ty: &ya_syn::token::TypeName) -> Self {
        match ty {
            ya_syn::token::TypeName::PrimType(prim_type) => Type::Prim(*prim_type),
            ya_syn::token::TypeName::Struct(_) => Type::Struct(vec![]),
            ya_syn::token::TypeName::Tuple(tys) => Type::Tuple(
                tys
                    .iter()
                    .map(|ty| ty.into())
                    .collect()
            ),
            ya_syn::token::TypeName::Array(ty) => {
                let len = if let Expr {
                    kind: ExprKind::Lit(lit),
                    ..
                } = LitExpr::parse_without_env(&ty.len) {
                    lit
                } else {
                    unreachable!();
                };

                Type::Array(ArrayType {
                    ty: Box::new(ty.ty.as_ref().into()),
                    len,
                })
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ArrayType {
    pub ty: Box<Type>,
    pub len: LitExpr,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConstInfo {
    pub ty: Type,
    pub expr: Expr,
}