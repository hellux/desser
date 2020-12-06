use super::*;

fn is_whitespace(c: char) -> bool {
    match c {
        '\n' | ' ' => true,
        _ => false,
    }
}

fn check_lexing(src: &str, expected: &str) {
    let actual: String = lex::tokenize(src)
        .map(|token| format!("{:?}", token))
        .collect::<String>()
        .chars()
        .filter(|c| !is_whitespace(*c))
        .collect();
    let expected: String = String::from(expected)
        .chars()
        .filter(|c| !is_whitespace(*c))
        .collect();
    assert_eq!(actual, expected);
}

#[test]
fn lex_floats() {
    // valid floats
    check_lexing("1.1", "Token { kind: Literal(Float(Decimal)), len: 3 }");
    check_lexing("1.1e0", "Token { kind: Literal(Float(Decimal)), len: 5 }");
    check_lexing("1.", "Token { kind: Literal(Float(Decimal)), len: 2 }");
    check_lexing(
        "234982374.239847238947",
        "Token { kind: Literal(Float(Decimal)), len: 22 }",
    );
    check_lexing(
        "0xabc.abc",
        "Token { kind: Literal(Float(Hexadecimal)), len: 9 }",
    );
    check_lexing("0o123.321", "Token { kind: Literal(Float(Octal)), len: 9 }");
    check_lexing("5e-5", "Token { kind: Literal(Float(Decimal)), len: 4 }");
    check_lexing("100E0", "Token { kind: Literal(Float(Decimal)), len: 5 }");

    // non-floats / invalid
    check_lexing(
        "1.e0",
        "Token { kind: Literal(Float(Decimal)), len: 2 }
         Token { kind: Ident, len: 2 }",
    );
    check_lexing("1.1e", "Token { kind: Literal(Invalid), len: 4 }");
    check_lexing(
        ".1",
        "Token { kind: Dot, len: 1 }
         Token { kind: Literal(Int(Decimal)), len: 1 }",
    );
}

#[test]
fn lex_integers() {
    // valid integers
    check_lexing("1", "Token { kind: Literal(Int(Decimal)), len: 1 }");
    check_lexing("0", "Token { kind: Literal(Int(Decimal)), len: 1 }");
    check_lexing("0b1", "Token { kind: Literal(Int(Binary)), len: 3 }");
    check_lexing("0xf", "Token { kind: Literal(Int(Hexadecimal)), len: 3 }");
    check_lexing("0o7", "Token { kind: Literal(Int(Octal)), len: 3 }");
    check_lexing(
        "0329487234",
        "Token { kind: Literal(Int(Decimal)), len: 10 }",
    );

    // non-integers / invalid
    check_lexing("ffff", "Token { kind: Ident, len: 4 }");
    check_lexing(
        "0b2",
        "Token { kind: Literal(Invalid), len: 2 }
         Token { kind: Literal(Int(Decimal)), len: 1}",
    );
}

/* */
#[test]
fn lex_comments() {
    // valid comments
    check_lexing(
        "// // line comment\n",
        "Token { kind: LineComment, len: 18 }
         Token { kind: Whitespace, len: 1}",
    );
    check_lexing(
        "/* block comment */",
        "Token { kind: BlockComment{ closed: true }, len: 19 }",
    );
    check_lexing(
        "/* /* /* /* nested block comment */ */ */ */",
        "Token { kind: BlockComment{ closed: true }, len: 44 }",
    );

    // invalid
    check_lexing(
        "/*",
        "Token { kind: BlockComment{ closed: false }, len: 2 }",
    );
    check_lexing(
        "/* /* /* /* nested block comment */ */ */",
        "Token { kind: BlockComment{ closed: false }, len: 41 }",
    );
    check_lexing(
        "/* */ */",
        "Token { kind: BlockComment{ closed: true }, len: 5 }
         Token { kind: Whitespace, len: 1 }
         Token { kind: Star, len: 1 }
         Token { kind: Slash, len: 1 }",
    );
}

#[test]
fn lex_strings() {
    // valid strings / chars
    check_lexing(
        r#""string""#,
        "Token { kind: Literal(Str { closed: true } ), len: 8 }"
    );
    check_lexing(
        "'char'",
        "Token { kind: Literal(Char { closed: true } ), len: 6 }"
    );
    check_lexing(
        r#""string with \\ escape""#,
        "Token { kind: Literal(Str { closed: true } ), len: 23 }"
    );
    check_lexing(
        r#"'char with \' escape'"#,
        "Token { kind: Literal(Char { closed: true } ), len: 21 }"
    );

    // invalid
    check_lexing(
        r#""unclosed string"#,
        "Token { kind: Literal(Str { closed: false } ), len: 16 }"
    );
    check_lexing(
        r#"'unclosed char \'"#,
        "Token { kind: Literal(Char { closed: false } ), len: 17 }"
    );
}

#[test]
fn lex_symbols() {
    check_lexing(
        "/;,.(){}[]?$=<>-&|+*^",
        "Token { kind: Slash, len: 1 }
         Token { kind: SemiColon, len: 1 }
         Token { kind: Comma, len: 1 }
         Token { kind: Dot, len: 1 }
         Token { kind: OpenParen, len: 1 }
         Token { kind: CloseParen, len: 1 }
         Token { kind: OpenBrace, len: 1 }
         Token { kind: CloseBrace, len: 1 }
         Token { kind: OpenBracket, len: 1 }
         Token { kind: CloseBracket, len: 1 }
         Token { kind: Question, len: 1 }
         Token { kind: Dollar, len: 1 }
         Token { kind: Eq, len: 1 }
         Token { kind: Lt, len: 1 }
         Token { kind: Gt, len: 1 }
         Token { kind: Minus, len: 1 }
         Token { kind: And, len: 1 }
         Token { kind: Or, len: 1 }
         Token { kind: Plus, len: 1 }
         Token { kind: Star, len: 1 }
         Token { kind: Caret, len: 1 }"
    );
}
