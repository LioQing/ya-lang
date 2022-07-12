use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LetDeclKind {
    pub mutable: Option<Spanned<()>>,
    pub id: SynResult<Spanned<String>>,
    pub ty: Option<SynResult<Spanned<String>>>,
}

pub type LetDecl = Spanned<LetDeclKind>;