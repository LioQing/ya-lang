use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ParamDeclKind {
    pub mutable: Option<Spanned<()>>,
    pub id: SynResult<Spanned<IdExpr>>,
    pub ty: SynResult<Spanned<IdExpr>>,
}

pub type ParamDecl = Spanned<ParamDeclKind>;