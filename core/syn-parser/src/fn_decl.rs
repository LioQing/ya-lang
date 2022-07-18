use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FnDeclKind {
    pub params: Vec<SynResult<ParamDecl>>,
    pub ret: Option<SynResult<Spanned<IdExpr>>>,
}

pub type FnDecl = Spanned<FnDeclKind>;