use super::*;

macro_rules! parse {
    ($code:literal) => {{
        let mut syn = Parser::new($code);
        syn.parse();
        syn.stack
    }};
}

macro_rules! assert_stack {
    ($stack:expr, $($patt:expr),* $(,)?) => {
        let mut stack = ($stack).into_iter();
        $(
            let x = &stack.next().unwrap_or(StackItem::None);
            assert!($patt.match_item(x), "exp: {:?}, found: {:?}", $patt, x);
        )*
    };

    (dbg $stack:expr, $($patt:expr),* $(,)?) => {
        let stack = $stack;
        println!("{:#?}", stack);
        let mut stack = stack.into_iter();
        $(
            let x = &stack.next().unwrap_or(StackItem::None);
            assert!($patt.match_item(x), "exp: {:?}, found: {:?}", $patt, x);
        )*
    };
}

#[test]
fn print() {
    println!("{:#?}", parse!("
        const my_fn = const b = () {
            let a = if a { 1 * (2 + *ptr) } else { b };
            a
        }
    "));
}

#[test]
fn id() {
    assert_stack!(
        parse!("a"),
        Patt::Id,
    );

    assert_stack!(
        parse!("abc123"),
        Patt::Id,
    );

    assert_stack!(
        parse!("_123"),
        Patt::Id,
    );
}

#[test]
fn lit() {
    assert_stack!(
        parse!("123"),
        Patt::Lit(LitPatt::Int),
    );

    assert_stack!(
        parse!("\"abc123\""),
        Patt::Lit(LitPatt::Quote),
    );

    assert_stack!(
        parse!("1.23"),
        Patt::Lit(LitPatt::Float),
    );

    assert_stack!(
        parse!("true"),
        Patt::Lit(LitPatt::Bool),
    );
}

#[test]
fn block() {
    assert_stack!(
        parse!("{}"),
        Patt::Block,
    );

    assert_stack!(
        dbg
        parse!("{;;;}"),
        Patt::Block,
    );

    assert_stack!(
        parse!("{a;;}"),
        Patt::Block,
    );

    assert_stack!(
        parse!("{
            a
        }"),
        Patt::Block,
    );

    assert_stack!(
        parse!("{
            a;
            b
        }"),
        Patt::Block,
    );
}