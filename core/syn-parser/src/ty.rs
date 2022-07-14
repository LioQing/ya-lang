use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TyKind {
    pub scope: Vec<SynResult<Spanned<String>>>,
    pub name: SynResult<Spanned<String>>,
}

pub type Ty = Spanned<TyKind>;