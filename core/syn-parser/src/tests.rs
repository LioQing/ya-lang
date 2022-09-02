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
    if (1} { 2 }
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

#[test]
fn paren() {
    assert_stack!(
        parse!("(a)"),
        Patt::Paren,
    );

    assert_stack!(
        parse!("(())"),
        Patt::Paren,
    );

    assert_stack!(
        parse!("(let a = 1)"),
        Patt::Paren,
    );

    assert_stack!(
        parse!("(a + b)"),
        Patt::Paren,
    );
}

#[test]
fn let_() {
    assert_stack!(
        parse!("let a = 1"),
        Patt::Let,
    );

    assert_stack!(
        parse!("let a"),
        Patt::Let,
    );

    assert_stack!(
        parse!("let a: i32"),
        Patt::Let,
    );

    assert_stack!(
        parse!("let a: i32 = 1"),
        Patt::Let,
    );
}

#[test]
fn const_() {
    assert_stack!(
        parse!("const a: i32 = 1"),
        Patt::Const,
    );
}

#[test]
fn fn_() {
    assert_stack!(
        parse!("() { 1 }"),
        Patt::Fn,
    );

    assert_stack!(
        parse!("() {}"),
        Patt::Fn,
    );

    assert_stack!(
        parse!("() { () }"),
        Patt::Fn,
    );

    assert_stack!(
        parse!("() -> i32 { 1 }"),
        Patt::Fn,
    );

    assert_stack!(
        parse!("(a: i32, b: i32) -> i32 { a + b }"),
        Patt::Fn,
    );

    assert_stack!(
        parse!("
            (a: i32, b: i32) -> i32 {
                a + b
            }
        "),
        Patt::Fn,
    );
}

#[test]
fn if_() {
    assert_stack!(
        parse!("if a { 1 }"),
        Patt::If,
    );

    assert_stack!(
        parse!("if a { 1 } else { 2 }"),
        Patt::If,
    );

    assert_stack!(
        parse!("if a { 1 } else if b { 2 }"),
        Patt::If,
    );

    assert_stack!(
        parse!("if a { 1 } else if b { 2 } else { 3 }"),
        Patt::If,
    );
}