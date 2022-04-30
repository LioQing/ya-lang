use super::*;

#[test]
fn eof() {
    let mut lexer = Lexer::new("");
    assert_eq!(lexer.next_token(), Ok(Token::Eof));
}

#[test]
fn basic_brackets() {
    let mut lexer = Lexer::new("() [] {}");

    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '(', depth: 0, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: ')', depth: 0, kind: BracketKind::Close }));

    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '[', depth: 0, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: ']', depth: 0, kind: BracketKind::Close }));

    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '{', depth: 0, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '}', depth: 0, kind: BracketKind::Close }));
}

#[test]
fn nested_brackets() {
    let mut lexer = Lexer::new("([([])] {})");

    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '(', depth: 0, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '[', depth: 1, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '(', depth: 2, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '[', depth: 3, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: ']', depth: 3, kind: BracketKind::Close }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: ')', depth: 2, kind: BracketKind::Close }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: ']', depth: 1, kind: BracketKind::Close }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '{', depth: 1, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '}', depth: 1, kind: BracketKind::Close }));
    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: ')', depth: 0, kind: BracketKind::Close }));
}

#[test]
fn separators() {
    let mut lexer = Lexer::new("; ,");

    assert_eq!(lexer.next_token(), Ok(Token::Separator { raw: ';' }));
    assert_eq!(lexer.next_token(), Ok(Token::Separator { raw: ',' }));
}

#[test]
fn missing_opening_bracket() {
    let mut lexer = Lexer::new("}");

    assert_eq!(lexer.next_token(), Err(Error::NoOpeningBracket { close: '}' }));
}

#[test]
fn imbalanced_brackets() {
    let mut lexer = Lexer::new("(}");

    assert_eq!(lexer.next_token(), Ok(Token::Bracket { raw: '(', depth: 0, kind: BracketKind::Open }));
    assert_eq!(lexer.next_token(), Err(Error::MismatchedBrackets { expected: ')', found: '}' }));
}

#[test]
fn integer() {
    let mut lexer = Lexer::new("123");

    assert_eq!(lexer.next_token(), Ok(Token::Numeric {
        raw: "123".to_owned(),
        prefix: "".to_owned(),
        suffix: "".to_owned(),
        kind: NumericKind::Integer,
    }));
}

#[test]
fn floats() {
    { // decimal, prefix
        let mut lexer = Lexer::new("0x123.456");

        assert_eq!(lexer.next_token(), Ok(Token::Numeric {
            raw: "123.456".to_owned(),
            prefix: "0x".to_owned(),
            suffix: "".to_owned(),
            kind: NumericKind::Float { dot_pos: Some(3), exp_pos: None },
        }));
    }

    { // decimal point first
        let mut lexer = Lexer::new(".1234");

        assert_eq!(lexer.next_token(), Ok(Token::Numeric {
            raw: ".1234".to_owned(),
            prefix: "".to_owned(),
            suffix: "".to_owned(),
            kind: NumericKind::Float { dot_pos: Some(0), exp_pos: None },
        }));
    }

    { // exponent, suffix
        let mut lexer = Lexer::new("123e456f32");

        assert_eq!(lexer.next_token(), Ok(Token::Numeric {
            raw: "123e456".to_owned(),
            prefix: "".to_owned(),
            suffix: "f32".to_owned(),
            kind: NumericKind::Float { dot_pos: None, exp_pos: Some(3) },
        }));
    }

    { // decimal + exponent, prefix + suffix
        let mut lexer = Lexer::new("0o123.456e7z");

        assert_eq!(lexer.next_token(), Ok(Token::Numeric {
            raw: "123.456e7".to_owned(),
            prefix: "0o".to_owned(),
            suffix: "z".to_owned(),
            kind: NumericKind::Float { dot_pos: Some(3), exp_pos: Some(7) },
        }));
    }
}

#[test]
fn radices() {
    { // base 16
        let mut lexer = Lexer::new("0x0123456789abcdef");

        assert_eq!(lexer.next_token(), Ok(Token::Numeric {
            raw: "0123456789abcdef".to_owned(),
            prefix: "0x".to_owned(),
            suffix: "".to_owned(),
            kind: NumericKind::Integer,
        }));
    }

    { // base 8
        let mut lexer = Lexer::new("0o0123456789abcdef");

        assert_eq!(lexer.next_token(), Ok(Token::Numeric {
            raw: "01234567".to_owned(),
            prefix: "0o".to_owned(),
            suffix: "89abcdef".to_owned(),
            kind: NumericKind::Integer,
        }));
    }

    { // base 2
        let mut lexer = Lexer::new("0b0123456789abcdef");

        assert_eq!(lexer.next_token(), Ok(Token::Numeric {
            raw: "01".to_owned(),
            prefix: "0b".to_owned(),
            suffix: "23456789abcdef".to_owned(),
            kind: NumericKind::Integer,
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
        assert_eq!(lexer.next_token(), Ok(Token::Identifier { raw: "zz".to_owned() }));
    }
}

#[test]
fn common_operators() {
    let mut lexer = Lexer::new(". + - * / << >> <> == != += <<= >>= ===");

    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: ".".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "+".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "-".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "*".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "/".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "<<".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: ">>".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "<>".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "==".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "!=".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "+=".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "<<=".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: ">>=".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Operator { raw: "===".to_owned() }));
}

#[test]
fn common_identifiers() {
    let mut lexer = Lexer::new("foo bar _field no_1 SCREAMING_SNAKE_CASE");

    assert_eq!(lexer.next_token(), Ok(Token::Identifier { raw: "foo".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Identifier { raw: "bar".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Identifier { raw: "_field".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Identifier { raw: "no_1".to_owned() }));
    assert_eq!(lexer.next_token(), Ok(Token::Identifier { raw: "SCREAMING_SNAKE_CASE".to_owned() }));
}

#[test]
fn normal_string() {
    let mut lexer = Lexer::new("\"foo\"");

    assert_eq!(lexer.next_token(), Ok(Token::StringChar {
        raw: "foo".to_owned(),
        prefix: "".to_owned(),
        suffix: "".to_owned(),
        quote: '"',
    }));
}

#[test]
fn string_with_prefix_and_suffix() {
    let mut lexer = Lexer::new("foo\"bar\"baz");

    assert_eq!(lexer.next_token(), Ok(Token::StringChar {
        raw: "bar".to_owned(),
        prefix: "foo".to_owned(),
        suffix: "baz".to_owned(),
        quote: '"',
    }));
}

#[test]
fn string_with_escape_sequence() {
    let mut lexer = Lexer::new("\"\\n\\r\\t\\\"\\'\\\\\"");

    assert_eq!(lexer.next_token(), Ok(Token::StringChar {
        raw: "\n\r\t\"'\\".to_owned(),
        prefix: "".to_owned(),
        suffix: "".to_owned(),
        quote: '"',
    }));
}

#[test]
fn char_with_prefix_suffix_escape_sequence() {
    let mut lexer = Lexer::new("abc'def\\n\\tghi'jkl");

    assert_eq!(lexer.next_token(), Ok(Token::StringChar {
        raw: "def\n\tghi".to_owned(),
        prefix: "abc".to_owned(),
        suffix: "jkl".to_owned(),
        quote: '\'',
    }));
}

#[test]
fn string_with_unknown_escape_sequence() {
    let mut lexer = Lexer::new("abc\"def\\zghi\"jkl");

    assert_eq!(lexer.next_token(), Err(Error::UnknownEscapeSequence {
        raw: "abc\"def\\zghi\"jkl".to_owned(),
        sequence: "\\z".to_owned()
    }));
}

#[test]
fn string_with_no_closing_quotation_mark() {
    let mut lexer = Lexer::new("abc\"def");

    assert_eq!(lexer.next_token(), Err(Error::NoClosingQuote { raw: "abc\"def".to_owned() }));
}