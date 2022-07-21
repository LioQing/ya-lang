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

    /** <pre> = <punc as pre op> <expr> */
    /** <suf> = <expr> <punc as suf op> */
    Un(UnExpr),

    /** <if> = if <expr of bool> <block> [else [<block> | <if>]]?*/
    If(IfExpr),
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

    pub fn un(self) -> UnExpr {
        match self {
            ExprKind::Un(un) => un,
            _ => panic!("not a un"),
        }
    }

    pub fn if_(self) -> IfExpr {
        match self {
            ExprKind::If(if_) => if_,
            _ => panic!("not an if"),
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct UnExpr {
    pub expr: Box<SynResult<Expr>>,
    pub pre: Vec<SynResult<Spanned<String>>>,
    pub suf: Vec<SynResult<Spanned<String>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct IfExpr {
    pub condition: Box<SynResult<Expr>>,
    pub body: Box<SynResult<Spanned<BlockExpr>>>,
    pub else_expr: Option<Box<SynResult<Expr>>>,
}
