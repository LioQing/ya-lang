use super::*;

// valid

#[test]
fn bracs() {
    let mut lexer = Lexer::new("()[({}){}]");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '(', depth: 0, kind: BracKind::Open }),
        Span::new(0, 0, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: ')', depth: 0, kind: BracKind::Close }),
        Span::new(0, 1, 1..2, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '[', depth: 0, kind: BracKind::Open }),
        Span::new(0, 2, 2..3, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '(', depth: 1, kind: BracKind::Open }),
        Span::new(0, 3, 3..4, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '{', depth: 2, kind: BracKind::Open }),
        Span::new(0, 4, 4..5, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '}', depth: 2, kind: BracKind::Close }),
        Span::new(0, 5, 5..6, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: ')', depth: 1, kind: BracKind::Close }),
        Span::new(0, 6, 6..7, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '{', depth: 1, kind: BracKind::Open }),
        Span::new(0, 7, 7..8, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '}', depth: 1, kind: BracKind::Close }),
        Span::new(0, 8, 8..9, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: ']', depth: 0, kind: BracKind::Close }),
        Span::new(0, 9, 9..10, 0),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn num_lit() {
    let mut lexer = Lexer::new("1234 1.23 12e34 0x1ab 0b101 123f32 0o123i32");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "1234".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Int,
        }),
        Span::new(0, 0, 0..4, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "1.23".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Float { dot_pos: Some(1), exp_pos: None },
        }),
        Span::new(0, 5, 5..9, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "12e34".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Float { dot_pos: None, exp_pos: Some(2) },
        }),
        Span::new(0, 10, 10..15, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "1ab".to_string(),
            prefix: "0x".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Int,
        }),
        Span::new(0, 16, 16..21, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "101".to_string(),
            prefix: "0b".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Int,
        }),
        Span::new(0, 22, 22..27, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "123".to_string(),
            prefix: "".to_string(),
            suffix: "f32".to_string(),
            kind: LitKind::Int,
        }),
        Span::new(0, 28, 28..34, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "123".to_string(),
            prefix: "0o".to_string(),
            suffix: "i32".to_string(),
            kind: LitKind::Int,
        }),
        Span::new(0, 35, 35..43, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn quote_lit() {
    let mut lexer = Lexer::new("ghi\"abc\"def ghi'abc'def");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "abc".to_string(),
            prefix: "ghi".to_string(),
            suffix: "def".to_string(),
            kind: LitKind::Quote { quote: '"' },
        }),
        Span::new(0, 0, 0..11, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "abc".to_string(),
            prefix: "ghi".to_string(),
            suffix: "def".to_string(),
            kind: LitKind::Quote { quote: '\'' },
        }),
        Span::new(0, 12, 12..23, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn bool_lit() {
    let mut lexer = Lexer::new("true false");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "true".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Bool,
        }),
        Span::new(0, 0, 0..4, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit(LitToken {
            raw: "false".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Bool,
        }),
        Span::new(0, 5, 5..10, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn id() {
    let mut lexer = Lexer::new("abc_123 _abc");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Id("abc_123".to_string()),
        Span::new(0, 0, 0..7, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Id("_abc".to_string()),
        Span::new(0, 8, 8..12, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn kw() {
    let mut lexer = Lexer::new("let const");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Kw("let".to_string()),
        Span::new(0, 0, 0..3, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Kw("const".to_string()),
        Span::new(0, 4, 4..9, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn op() {
    let mut lexer = Lexer::new("- + += !&! *!&@");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Op("-".to_owned()),
        Span::new(0, 0, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Op("+".to_owned()),
        Span::new(0, 2, 2..3, 1),
    ))));
    
    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Op("+=".to_owned()),
        Span::new(0, 4, 4..6, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Op("!&!".to_owned()),
        Span::new(0, 7, 7..10, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Op("*!&@".to_owned()),
        Span::new(0, 11, 11..15, 1),
    ))));
}

#[test]
fn punc() {
    let mut lexer = Lexer::new("; .");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Punc(";".to_owned()),
        Span::new(0, 0, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Punc(".".to_owned()),
        Span::new(0, 2, 2..3, 1),
    ))));
}

// errors

#[test]
fn mismatched_bracs() {
    let mut lexer = Lexer::new("{)");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '{', depth: 0, kind: BracKind::Open }),
        Span::new(0, 0, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), Some(Err(Error::new(
        ErrorKind::MismatchedBracs('}', ')'),
        Span::new(0, 1, 1..2, 0),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn missing_open_brac() {
    let mut lexer = Lexer::new("}");

    assert_eq!(lexer.next(), Some(Err(Error::new(
        ErrorKind::MissingOpenBrac('}'),
        Span::new(0, 0, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn missing_close_brac() {
    let mut lexer = Lexer::new("{");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Brac(BracToken { raw: '{', depth: 0, kind: BracKind::Open }),
        Span::new(0, 0, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), Some(Err(Error::new(
        ErrorKind::MissingCloseBrac('{'),
        Span::new(0, 0, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn missing_digit_after_prefix() {
    let mut lexer = Lexer::new("0x");

    assert_eq!(lexer.next(), Some(Err(Error::new(
        ErrorKind::MissingDigitAfterPrefix("0x".to_owned()),
        Span::new(0, 0, 0..2, 0),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn missing_close_quote() {
    let mut lexer = Lexer::new("\"abc");

    assert_eq!(lexer.next(), Some(Err(Error::new(
        ErrorKind::MissingCloseQuote("\"abc".to_owned()),
        Span::new(0, 0, 0..4, 0),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn invalid_esc_seq() {
    // single
    let mut lexer = Lexer::new("\"\\z\"");

    assert_eq!(lexer.next(), Some(Err(Error::new(
        ErrorKind::InvalidEscSeq(vec!["\\z".to_owned()]),
        Span::new(0, 0, 0..4, 0),
    ))));

    assert_eq!(lexer.next(), None);

    // multiple
    let mut lexer = Lexer::new("\"\\z\\a\\1\"");

    assert_eq!(lexer.next(), Some(Err(Error::new(
        ErrorKind::InvalidEscSeq(vec!["\\z".to_owned(), "\\a".to_owned(), "\\1".to_owned()]),
        Span::new(0, 0, 0..8, 0),
    ))));

    assert_eq!(lexer.next(), None);
}
