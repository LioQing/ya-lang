use super::*;

#[test]
fn eof() {
    let mut lexer = Lexer::new("");
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Eof, .. })));
}

#[test]
fn basic_brackets() {
    let mut lexer = Lexer::new("() [] {}");

    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '(', depth: 0, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: ')', depth: 0, kind: BracketKind::Close }, .. })));

    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '[', depth: 0, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: ']', depth: 0, kind: BracketKind::Close }, .. })));

    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '{', depth: 0, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '}', depth: 0, kind: BracketKind::Close }, .. })));
}

#[test]
fn nested_brackets() {
    let mut lexer = Lexer::new("([([])] {})");

    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '(', depth: 0, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '[', depth: 1, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '(', depth: 2, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '[', depth: 3, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: ']', depth: 3, kind: BracketKind::Close }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: ')', depth: 2, kind: BracketKind::Close }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: ']', depth: 1, kind: BracketKind::Close }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '{', depth: 1, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '}', depth: 1, kind: BracketKind::Close }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: ')', depth: 0, kind: BracketKind::Close }, .. })));
}

#[test]
fn separators() {
    let mut lexer = Lexer::new("; ,");

    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Separator { raw: ';' }, .. })));
    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Separator { raw: ',' }, .. })));
}

#[test]
fn missing_opening_bracket() {
    let mut lexer = Lexer::new("}");

    assert!(matches!(lexer.next_token(), Err(Error::NoOpeningBracket { close: '}' })));
}

#[test]
fn imbalanced_brackets() {
    let mut lexer = Lexer::new("(}");

    assert!(matches!(lexer.next_token(), Ok(Token { kind: TokenKind::Bracket { raw: '(', depth: 0, kind: BracketKind::Open }, .. })));
    assert!(matches!(lexer.next_token(), Err(Error::MismatchedBrackets { expected: ')', found: '}' })));
}

#[test]
fn integer() {
    let mut lexer = Lexer::new("123");

    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Numeric {
            raw: "123".to_owned(),
            prefix: "".to_owned(),
            suffix: "".to_owned(),
            kind: NumericKind::Integer,
        },
        span: Span { line: 1, col_range: 0..3, codepoint_range: 0..3, dist_from_prev: 0 },
    }));
}

#[test]
fn floats() {
    { // decimal, prefix
        let mut lexer = Lexer::new("0x123.456");

        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Numeric {
                raw: "123.456".to_owned(),
                prefix: "0x".to_owned(),
                suffix: "".to_owned(),
                kind: NumericKind::Float { dot_pos: Some(3), exp_pos: None },
            },
            span: Span { line: 1, col_range: 0..9, codepoint_range: 0..9, dist_from_prev: 0 },
        }));
    }

    { // decimal point first
        let mut lexer = Lexer::new(".1234");

        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Numeric {
                raw: ".1234".to_owned(),
                prefix: "".to_owned(),
                suffix: "".to_owned(),
                kind: NumericKind::Float { dot_pos: Some(0), exp_pos: None },
            },
            span: Span { line: 1, col_range: 0..5, codepoint_range: 0..5, dist_from_prev: 0 },
        }));
    }

    { // exponent, suffix
        let mut lexer = Lexer::new("123e456f32");

        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Numeric {
                raw: "123e456".to_owned(),
                prefix: "".to_owned(),
                suffix: "f32".to_owned(),
                kind: NumericKind::Float { dot_pos: None, exp_pos: Some(3) },
            },
            span: Span { line: 1, col_range: 0..10, codepoint_range: 0..10, dist_from_prev: 0 },
        }));
    }

    { // decimal + exponent, prefix + suffix
        let mut lexer = Lexer::new("0o123.456e7z");

        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Numeric {
                raw: "123.456e7".to_owned(),
                prefix: "0o".to_owned(),
                suffix: "z".to_owned(),
                kind: NumericKind::Float { dot_pos: Some(3), exp_pos: Some(7) },
            },
            span: Span { line: 1, col_range: 0..12, codepoint_range: 0..12, dist_from_prev: 0 },
        }));
    }
}

#[test]
fn radices() {
    { // base 16
        let mut lexer = Lexer::new("0x0123456789abcdef");

        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Numeric {
                raw: "0123456789abcdef".to_owned(),
                prefix: "0x".to_owned(),
                suffix: "".to_owned(),
                kind: NumericKind::Integer,
            },
            span: Span { line: 1, col_range: 0..18, codepoint_range: 0..18, dist_from_prev: 0 },
        }));
    }

    { // base 8
        let mut lexer = Lexer::new("0o0123456789abcdef");

        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Numeric {
                raw: "01234567".to_owned(),
                prefix: "0o".to_owned(),
                suffix: "89abcdef".to_owned(),
                kind: NumericKind::Integer,
            },
            span: Span { line: 1, col_range: 0..18, codepoint_range: 0..18, dist_from_prev: 0 },
        }));
    }

    { // base 2
        let mut lexer = Lexer::new("0b0123456789abcdef");

        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Numeric {
                raw: "01".to_owned(),
                prefix: "0b".to_owned(),
                suffix: "23456789abcdef".to_owned(),
                kind: NumericKind::Integer,
            },
            span: Span { line: 1, col_range: 0..18, codepoint_range: 0..18, dist_from_prev: 0 },
        }));
    }
}

#[test]
fn missing_valid_digits_after_numeric_prefix() {
    { // empty after prefix
        let mut lexer = Lexer::new("0x");
        assert_eq!(lexer.next_token(), Err(Error::NoDigitsAfterNumericPrefix { prefix: "0x".to_owned() }));
    }

    { // invalid digit after prefix
        let mut lexer = Lexer::new("0xzz");
        assert_eq!(lexer.next_token(), Err(Error::NoDigitsAfterNumericPrefix { prefix: "0x".to_owned() }));
        assert_eq!(lexer.next_token(), Ok(Token {
            kind: TokenKind::Identifier { raw: "zz".to_owned() },
            span: Span { line: 1, col_range: 2..4, codepoint_range: 2..4, dist_from_prev: 0 },
        }));
    }
}

#[test]
fn common_operators() {
    let mut lexer = Lexer::new(". + - * / << >> <> == != += <<= >>= ===");

    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: ".".to_owned() },
        span: Span { line: 1, col_range: 0..1, codepoint_range: 0..1, dist_from_prev: 0 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "+".to_owned() },
        span: Span { line: 1, col_range: 2..3, codepoint_range: 2..3, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "-".to_owned() },
        span: Span { line: 1, col_range: 4..5, codepoint_range: 4..5, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "*".to_owned() },
        span: Span { line: 1, col_range: 6..7, codepoint_range: 6..7, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "/".to_owned() },
        span: Span { line: 1, col_range: 8..9, codepoint_range: 8..9, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "<<".to_owned() },
        span: Span { line: 1, col_range: 10..12, codepoint_range: 10..12, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: ">>".to_owned() },
        span: Span { line: 1, col_range: 13..15, codepoint_range: 13..15, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "<>".to_owned() },
        span: Span { line: 1, col_range: 16..18, codepoint_range: 16..18, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "==".to_owned() },
        span: Span { line: 1, col_range: 19..21, codepoint_range: 19..21, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "!=".to_owned() },
        span: Span { line: 1, col_range: 22..24, codepoint_range: 22..24, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "+=".to_owned() },
        span: Span { line: 1, col_range: 25..27, codepoint_range: 25..27, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "<<=".to_owned() },
        span: Span { line: 1, col_range: 28..31, codepoint_range: 28..31, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: ">>=".to_owned() },
        span: Span { line: 1, col_range: 32..35, codepoint_range: 32..35, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Operator { raw: "===".to_owned() },
        span: Span { line: 1, col_range: 36..39, codepoint_range: 36..39, dist_from_prev: 1 },
    }));
}

#[test]
fn common_identifiers() {
    let mut lexer = Lexer::new("foo bar _field no_1 SCREAMING_SNAKE_CASE");

    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Identifier { raw: "foo".to_owned() },
        span: Span { line: 1, col_range: 0..3, codepoint_range: 0..3, dist_from_prev: 0 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Identifier { raw: "bar".to_owned() },
        span: Span { line: 1, col_range: 4..7, codepoint_range: 4..7, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Identifier { raw: "_field".to_owned() },
        span: Span { line: 1, col_range: 8..14, codepoint_range: 8..14, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Identifier { raw: "no_1".to_owned() },
        span: Span { line: 1, col_range: 15..19, codepoint_range: 15..19, dist_from_prev: 1 },
    }));
    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::Identifier { raw: "SCREAMING_SNAKE_CASE".to_owned() },
        span: Span { line: 1, col_range: 20..40, codepoint_range: 20..40, dist_from_prev: 1 },
    }));
}

#[test]
fn normal_string() {
    let mut lexer = Lexer::new("\"foo\"");

    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::StringChar {
            raw: "foo".to_owned(),
            prefix: "".to_owned(),
            suffix: "".to_owned(),
            quote: '"',
        },
        span: Span { line: 1, col_range: 0..5, codepoint_range: 0..5, dist_from_prev: 0 },
    }));
}

#[test]
fn string_with_prefix_and_suffix() {
    let mut lexer = Lexer::new("foo\"bar\"baz");

    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::StringChar {
            raw: "bar".to_owned(),
            prefix: "foo".to_owned(),
            suffix: "baz".to_owned(),
            quote: '"',
        },
        span: Span { line: 1, col_range: 0..11, codepoint_range: 0..11, dist_from_prev: 0 },
    }));
}

#[test]
fn string_with_escape_sequence() {
    let mut lexer = Lexer::new("\"\\n\\r\\t\\\"\\'\\\\\"");

    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::StringChar {
            raw: "\n\r\t\"'\\".to_owned(),
            prefix: "".to_owned(),
            suffix: "".to_owned(),
            quote: '"',
        },
        span: Span { line: 1, col_range: 0..14, codepoint_range: 0..14, dist_from_prev: 0 },
    }));
}

#[test]
fn char_with_prefix_suffix_escape_sequence() {
    let mut lexer = Lexer::new("abc'def\\n\\tghi'jkl");

    assert_eq!(lexer.next_token(), Ok(Token {
        kind: TokenKind::StringChar {
            raw: "def\n\tghi".to_owned(),
            prefix: "abc".to_owned(),
            suffix: "jkl".to_owned(),
            quote: '\'',
        },
        span: Span { line: 1, col_range: 0..18, codepoint_range: 0..18, dist_from_prev: 0 },
    }));
}

#[test]
fn string_with_unknown_escape_sequence() {
    let mut lexer = Lexer::new("abc\"def\\zghi\"jkl");

    assert_eq!(lexer.next_token(), Err(Error::UnknownEscapeSequence {
        raw: "abc\"def\\zghi\"jkl".to_owned(),
        sequence: "\\z".to_owned(),
    }));
}

#[test]
fn string_with_no_closing_quotation_mark() {
    let mut lexer = Lexer::new("abc\"def");

    assert_eq!(lexer.next_token(), Err(Error::NoClosingQuote { raw: "abc\"def".to_owned() }));
}