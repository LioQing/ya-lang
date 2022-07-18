use super::*;
use paste::paste;

macro_rules! stack_items {
    ($($name:ident => $typename:ty,)*) => {
        #[derive(Debug, PartialEq, Eq, Clone, Hash)]
        pub enum StackItem {
            $($name($typename),)*
            Err(Error),
        }
        
        impl StackItem {
            pub fn span(&self) -> &token::Span {
                match &self {
                    $(Self::$name(token::Spanned { span, .. }))|*
                    | &Self::Err(Error { span, .. }) => {
                        span
                    }
                }
            }

            paste! {
                $(
                    pub fn [<$name:snake _or_err>](self) -> Result<$typename, Error> {
                        match self {
                            Self::$name(x) => Ok(x),
                            Self::Err(err) => Err(err),
                            _ => panic!("not a {} or error", stringify!([<$name:snake>])),
                        }
                    }
                )*
            }
        }

        $(
            impl From<Result<$typename, Error>> for StackItem {
                fn from(x: Result<$typename, Error>) -> Self {
                    match x {
                        Ok(x) => Self::$name(x),
                        Err(err) => Self::Err(err),
                    }
                }
            }
        )*
    };

    ($($name:ident => $typename:ty),*) => {
        stack_items! { $($name,)* }
    };
}

stack_items! {
    Expr => Expr,
    Stmts => Repeats<ExprKind>,
    LetDecl => LetDecl,
    ScopedId => Repeats<IdExpr>,
    ParamDecl => ParamDecl,
    Params => Repeats<ParamDeclKind>,
    Token => Token,
}
