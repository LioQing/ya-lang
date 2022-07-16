use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LetDeclKind {
    pub mutable: Option<Spanned<()>>,
    pub id: SynResult<Spanned<IdExpr>>,
    pub ty: Option<SynResult<Spanned<IdExpr>>>,
}

pub type LetDecl = Spanned<LetDeclKind>;