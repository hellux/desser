use super::*;

fn is_whitespace(c: char) -> bool {
    match c {
        '\n' | ' ' => true,
        _ => false,
    }
}

fn check_lexing(src: &str, expected: &str) {
    let actual: String = raw::tokenize(src)
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

fn check_cooking(src: &str, expected: &str) {
    let actual: String = cook::tokenize(src)
        .map(|(_, token)| format!("{:?}", token.kind))
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
        "Token { kind: Literal(Str { closed: true } ), len: 8 }",
    );
    check_lexing(
        "'char'",
        "Token { kind: Literal(Char { closed: true } ), len: 6 }",
    );
    check_lexing(
        r#""string with \\ escape""#,
        "Token { kind: Literal(Str { closed: true } ), len: 23 }",
    );
    check_lexing(
        r#"'char with \' escape'"#,
        "Token { kind: Literal(Char { closed: true } ), len: 21 }",
    );

    // invalid
    check_lexing(
        r#""unclosed string"#,
        "Token { kind: Literal(Str { closed: false } ), len: 16 }",
    );
    check_lexing(
        r#"'unclosed char \'"#,
        "Token { kind: Literal(Char { closed: false } ), len: 17 }",
    );
}

#[test]
fn cook_integers() {
    check_cooking("0xff", "Literal(Int(255)) Eof");
    check_cooking("0x", "Literal(Int(0)) Eof");
}

#[test]
fn cook_structs() {
    check_cooking(
        r#"
    def psf2_header {
        [u8; 4] magic ~ "head",
        u32 version ~ <= 0,
        until headersize ignore,
    }"#,
        "
        Keyword(Def)
        Ident(0)
        OpenDelim(Brace)
        OpenDelim(Bracket)
        Ident(1)
        SemiColon
        Literal(Int(4))
        CloseDelim(Bracket)
        Ident(2)
        Tilde
        Literal(Str([104,101,97,100]))
        Comma
        Ident(3)
        Ident(4)
        Tilde
        Leq
        Literal(Int(0))
        Comma
        Ident(5)
        Ident(6)
        Ident(7)
        Comma
        CloseDelim(Brace)
        Eof",
    );
}
