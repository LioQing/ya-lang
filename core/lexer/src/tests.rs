use super::*;

#[test]
fn parens() {
    let mut lexer = Lexer::new("()[({}){}]");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: '(', depth: 0, kind: ParenKind::Open },
        Span::new(0, 0..1, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: ')', depth: 0, kind: ParenKind::Close },
        Span::new(0, 1..2, 1..2, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: '[', depth: 0, kind: ParenKind::Open },
        Span::new(0, 2..3, 2..3, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: '(', depth: 1, kind: ParenKind::Open },
        Span::new(0, 3..4, 3..4, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: '{', depth: 2, kind: ParenKind::Open },
        Span::new(0, 4..5, 4..5, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: '}', depth: 2, kind: ParenKind::Close },
        Span::new(0, 5..6, 5..6, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: ')', depth: 1, kind: ParenKind::Close },
        Span::new(0, 6..7, 6..7, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: '{', depth: 1, kind: ParenKind::Open },
        Span::new(0, 7..8, 7..8, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: '}', depth: 1, kind: ParenKind::Close },
        Span::new(0, 8..9, 8..9, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Paren { raw: ']', depth: 0, kind: ParenKind::Close },
        Span::new(0, 9..10, 9..10, 0),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn num_lit() {
    let mut lexer = Lexer::new("1234 1.23 12e34 0x1ab 0b101 123f32 0o123i32");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "1234".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Integer,
        },
        Span::new(0, 0..4, 0..4, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "1.23".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Float { dot_pos: Some(1), exp_pos: None },
        },
        Span::new(0, 5..9, 5..9, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "12e34".to_string(),
            prefix: "".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Float { dot_pos: None, exp_pos: Some(2) },
        },
        Span::new(0, 10..15, 10..15, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "1ab".to_string(),
            prefix: "0x".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Integer,
        },
        Span::new(0, 16..21, 16..21, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "101".to_string(),
            prefix: "0b".to_string(),
            suffix: "".to_string(),
            kind: LitKind::Integer,
        },
        Span::new(0, 22..27, 22..27, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "123".to_string(),
            prefix: "".to_string(),
            suffix: "f32".to_string(),
            kind: LitKind::Integer,
        },
        Span::new(0, 28..34, 28..34, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "123".to_string(),
            prefix: "0o".to_string(),
            suffix: "i32".to_string(),
            kind: LitKind::Integer,
        },
        Span::new(0, 35..43, 35..43, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn quote_lit() {
    let mut lexer = Lexer::new("ghi\"abc\"def ghi'abc'def");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "abc".to_string(),
            prefix: "ghi".to_string(),
            suffix: "def".to_string(),
            kind: LitKind::Quote { quote: '"' },
        },
        Span::new(0, 0..11, 0..11, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Lit {
            raw: "abc".to_string(),
            prefix: "ghi".to_string(),
            suffix: "def".to_string(),
            kind: LitKind::Quote { quote: '\'' },
        },
        Span::new(0, 12..23, 12..23, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn id() {
    let mut lexer = Lexer::new("abc_123 _abc");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Id { raw: "abc_123".to_string() },
        Span::new(0, 0..7, 0..7, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Id { raw: "_abc".to_string() },
        Span::new(0, 8..12, 8..12, 1),
    ))));

    assert_eq!(lexer.next(), None);
}

#[test]
fn punc() {
    let mut lexer = Lexer::new("- + += !&! .*!&@");

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Punc { raw: "-".to_owned() },
        Span::new(0, 0..1, 0..1, 0),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Punc { raw: "+".to_owned() },
        Span::new(0, 2..3, 2..3, 1),
    ))));
    
    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Punc { raw: "+=".to_owned() },
        Span::new(0, 4..6, 4..6, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Punc { raw: "!&!".to_owned() },
        Span::new(0, 7..10, 7..10, 1),
    ))));

    assert_eq!(lexer.next(), Some(Ok(Token::new(
        TokenKind::Punc { raw: ".*!&@".to_owned() },
        Span::new(0, 11..16, 11..16, 1),
    ))));
}