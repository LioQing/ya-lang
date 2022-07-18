use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ExprKind {
    /** <lit> */
    Lit(LitToken),

    /** <id> */
    Id(IdExpr),

    /** <paren> = (<expr>) */
    Paren(ParenExpr),

    /** <block> = { [<expr>];* [<expr>]? } */
    Block(BlockExpr),

    /** <let> = let [mut]? <id> [: <ty>]? [= <expr>]? */
    Let(LetExpr),

    /** <const> = const <id> [: <built-in ty>]? = <const expr> */
    Const(ConstExpr),

    /** <fn> = ([[mut]? <id>: <ty>],*) [-> <ty>]? [<block> | => <expr>] */
    Fn(FnExpr),

    /** <bin> = <expr> <punc as bin op> <expr> */
    Bin(BinExpr),
}

impl ExprKind {
    pub fn lit(self) -> LitToken {
        match self {
            ExprKind::Lit(lit) => lit,
            _ => panic!("not a lit"),
        }
    }

    pub fn id(self) -> IdExpr {
        match self {
            ExprKind::Id(id) => id,
            _ => panic!("not an id"),
        }
    }

    pub fn paren(self) -> ParenExpr {
        match self {
            ExprKind::Paren(paren) => paren,
            _ => panic!("not a paren"),
        }
    }

    pub fn block(self) -> BlockExpr {
        match self {
            ExprKind::Block(block) => block,
            _ => panic!("not a block"),
        }
    }

    pub fn let_(self) -> LetExpr {
        match self {
            ExprKind::Let(let_) => let_,
            _ => panic!("not a let"),
        }
    }

    pub fn const_(self) -> ConstExpr {
        match self {
            ExprKind::Const(const_) => const_,
            _ => panic!("not a const"),
        }
    }

    pub fn bin(self) -> BinExpr {
        match self {
            ExprKind::Bin(bin) => bin,
            _ => panic!("not a bin"),
        }
    }
}

pub type Expr = Spanned<ExprKind>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct IdExpr {
    pub id: Spanned<String>,
    pub scope: Vec<Spanned<String>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ParenExpr {
    pub expr: Box<SynResult<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct BlockExpr {
    pub stmts: Vec<SynResult<Expr>>,
    pub expr: Option<Box<SynResult<Expr>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LetExpr {
    pub mutable: Option<Spanned<()>>,
    pub id: SynResult<Spanned<IdExpr>>,
    pub ty: Option<SynResult<Spanned<IdExpr>>>,
    pub expr: Option<Box<SynResult<Expr>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ConstExpr {
    pub id: SynResult<Spanned<IdExpr>>,
    pub ty: Option<SynResult<Spanned<IdExpr>>>,
    pub expr: Box<SynResult<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FnExpr {
    pub decl: SynResult<FnDecl>,
    pub body: Box<SynResult<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct BinExpr {
    pub lhs: Box<SynResult<Expr>>,
    pub op: SynResult<Spanned<String>>,
    pub rhs: Box<SynResult<Expr>>,
}
