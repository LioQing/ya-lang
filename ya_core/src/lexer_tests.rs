use super::*;

#[test]
fn eof() {
    let mut lexer = Lexer::new("");
    assert_eq!(lexer.next_token(), Ok(Token::Eof));
}

#[test]
fn basic_brackets() {
    let mut lexer = Lexer::new("() [] {}");

    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '(', kind: PunctuationKind::Open(0) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: ')', kind: PunctuationKind::Close(0) }));

    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '[', kind: PunctuationKind::Open(0) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: ']', kind: PunctuationKind::Close(0) }));

    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '{', kind: PunctuationKind::Open(0) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '}', kind: PunctuationKind::Close(0) }));
}

#[test]
fn nested_brackets() {
    let mut lexer = Lexer::new("([([])] {})");

    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '(', kind: PunctuationKind::Open(0) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '[', kind: PunctuationKind::Open(1) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '(', kind: PunctuationKind::Open(2) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '[', kind: PunctuationKind::Open(3) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: ']', kind: PunctuationKind::Close(3) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: ')', kind: PunctuationKind::Close(2) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: ']', kind: PunctuationKind::Close(1) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '{', kind: PunctuationKind::Open(1) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '}', kind: PunctuationKind::Close(1) }));
    assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: ')', kind: PunctuationKind::Close(0) }));
}

#[test]
fn imbalanced_brackets() {
    { // no opening bracket
        let mut lexer = Lexer::new("}");

        assert_eq!(lexer.next_token(), Err(LexerError::ImbalancedBrackets { expected: ' ', found: '}' }));
    }

    { // mismatch brackets
        let mut lexer = Lexer::new("(}");

        assert_eq!(lexer.next_token(), Ok(Token::Punctuation { raw: '(', kind: PunctuationKind::Open(0) }));
        assert_eq!(lexer.next_token(), Err(LexerError::ImbalancedBrackets { expected: ')', found: '}' }));
    }
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
fn no_valid_digits_after_numeric_prefix() {
    { // empty after prefix
        let mut lexer = Lexer::new("0x");
        assert_eq!(lexer.next_token(), Err(LexerError::NoValidDigitsAfterNumericPrefix { prefix: "0x".to_owned() }));
    }

    { // invalid digit after prefix
        let mut lexer = Lexer::new("0xzz");
        assert_eq!(lexer.next_token(), Err(LexerError::NoValidDigitsAfterNumericPrefix { prefix: "0x".to_owned() }));
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