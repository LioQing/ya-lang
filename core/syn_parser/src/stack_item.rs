use super::*;
use paste::paste;

macro_rules! stack_items {
    ($($name:ident,)*) => {
        #[derive(Debug, PartialEq, Eq, Clone, Hash)]
        pub enum StackItem {
            $($name($name),)*
            Err(Error),
        }
        
        impl StackItem {
            pub fn span(&self) -> &Span {
                match &self {
                    $(Self::$name($name { span, .. }))|*
                    | &Self::Err(Error { span, .. }) => {
                        span
                    }
                }
            }

            paste! {
                $(
                    pub fn [<$name:lower _or_err>](self) -> Result<$name, Error> {
                        match self {
                            Self::$name(x) => Ok(x),
                            Self::Err(err) => Err(err),
                            _ => panic!("not a {} or error", stringify!([<$name:lower>])),
                        }
                    }
                )*
            }
        }

        $(
            impl From<Result<$name, Error>> for StackItem {
                fn from(x: Result<$name, Error>) -> Self {
                    match x {
                        Ok(x) => Self::$name(x),
                        Err(err) => Self::Err(err),
                    }
                }
            }
        )*
    };

    ($($name:ident),*) => {
        stack_item_gen! { $($name,)* }
    };
}

stack_items! {
    Expr,
    Stmts,
    Stmt,
    Token,
}
