use std::collections::HashMap;

use crate::spec::error;
use crate::spec::ast;
use crate::spec::lex::Delim::{Brace, Bracket, Paren};
use crate::spec::lex::Spacing::{Alone, Joint};
use crate::spec::lex::{
    Delim, DelimNode, Keyword, LitKind, Spacing, TokKind, TokTree, Token,
    TokenStream,
};
use crate::sym;

use self::PErrorKind::*;

#[derive(Clone, Debug)]
enum PErrorKind {
    Unexpected,
    UnexpectedToken(TokKind),
    UnexpectedOpenDelim(Delim),
    UnexpectedCloseDelimOrEof,
    InvalidSpacing(Spacing),
}

#[derive(Clone, Debug)]
struct PError {
    kind: PErrorKind,
    pos: u32,
    hint: Option<&'static str>,
}

type PResult<T> = Result<T, PError>;

impl From<PError> for error::Error {
    fn from(p: PError) -> Self {
        let start = p.pos;
        let end = None;
        let desc = match p.kind {
            Unexpected => format!("unexpected token or delimiter"),
            UnexpectedToken(token) => {
                format!("unexpected token -- {:?}", token)
            }
            UnexpectedOpenDelim(delim) => {
                format!("unexpected opening delimiter -- {:?}", delim)
            }
            UnexpectedCloseDelimOrEof => {
                format!("unexpected closing delimiter")
            }
            InvalidSpacing(actual) => match actual {
                Joint => format!("there cannot be whitespace here"),
                Alone => format!("there has to be whitespace here"),
            },
        };
        let hint = p.hint;

        error::Error {
            start,
            end,
            desc,
            hint,
            ty: error::ErrorType::Parsing,
        }
    }
}

pub fn parse_file_spec(
    symtab: sym::SymbolTable,
    mut tokens: TokenStream,
) -> (
    Result<ast::FileSpecification, error::Error>,
    sym::SymbolTable,
    Vec<error::Error>,
) {
    let mut parser = Parser::new(symtab);

    let file_spec = parser.parse_spec(&mut tokens);
    let (symtab, errors) = parser.consume();

    (
        file_spec.map_err(|pe| pe.into()),
        symtab,
        errors.into_iter().map(|pe| pe.into()).collect(),
    )
}

struct Parser {
    symtab: sym::SymbolTable,

    tree: TokTree,
    spacing: Spacing,
    pos: u32,

    delim_span: error::Span,

    errors: Vec<PError>,
}

impl Parser {
    fn new(symtab: sym::SymbolTable) -> Self {
        Parser {
            symtab,
            tree: TokTree::Token(Token::dummy()),
            spacing: Joint,
            pos: 0,
            delim_span: error::Span(0, 0),
            errors: Vec::new(),
        }
    }

    fn eat(&mut self, stream: &mut TokenStream) -> PResult<()> {
        if stream.not_empty() {
            let (tree, spacing) = stream.eat().unwrap();

            let pos = match &tree {
                TokTree::Token(token) => token.span.0,
                TokTree::Delim(dn) => dn.span.0,
            };

            self.tree = tree;
            self.spacing = spacing;
            self.pos = pos;

            Ok(())
        } else {
            self.pos = self.delim_span.1;
            Err(self.err(UnexpectedCloseDelimOrEof))
        }
    }

    fn expect_token(&mut self) -> PResult<Token> {
        match self.tree.take() {
            TokTree::Token(token) => Ok(token),
            TokTree::Delim(dn) => Err(self.err(UnexpectedOpenDelim(dn.delim))),
        }
    }

    fn expect_kind(&mut self, kind: TokKind) -> PResult<()> {
        let token = self.expect_token()?;

        match token.kind {
            k if k == kind => Ok(()),
            k => Err(self.err(UnexpectedToken(k))),
        }
    }

    fn expect_keyword(&mut self) -> PResult<Keyword> {
        let keyword_token = self.expect_token()?;

        match keyword_token.kind {
            TokKind::Keyword(kw) => Ok(kw),
            kind => {
                Err(self.err_hint(UnexpectedToken(kind), "expected a keyword"))
            }
        }
    }

    fn expect_ident(&mut self) -> PResult<sym::Sym> {
        let id_token = self.expect_token()?;

        match id_token.kind {
            TokKind::Ident(id) => Ok(id),
            kind => {
                Err(self
                    .err_hint(UnexpectedToken(kind), "expected identifier"))
            }
        }
    }

    fn expect_delim(&mut self) -> PResult<DelimNode> {
        match self.tree.take() {
            TokTree::Delim(dn) => {
                self.delim_span = dn.span;
                Ok(dn)
            }
            TokTree::Token(token) => Err(self.err_hint(
                UnexpectedToken(token.kind),
                "expected opening delimiter",
            )),
        }
    }

    fn assert_spacing(&mut self, spacing: Spacing, hint: &'static str) {
        if self.spacing != spacing {
            self.errors
                .push(self.err_hint(InvalidSpacing(self.spacing), hint));
        }
    }

    fn assert_eof(&mut self, stream: &TokenStream, hint: &'static str) {
        match stream.peek() {
            Some(_) => self.errors.push(self.err_hint(Unexpected, hint)),
            None => {}
        }
    }

    fn parse_spec(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<ast::FileSpecification> {
        let mut structs = HashMap::new();

        while stream.not_empty() {
            self.eat(stream)?;
            let kw = self.expect_keyword()?;

            match kw {
                Keyword::Def => {
                    let (id, st) = self.parse_struct(stream)?;
                    structs.insert(id, st);
                }
                Keyword::Set => unimplemented!(),
            }
        }

        Ok(ast::FileSpecification {
            structs,
            constants: sym::Namespace::new(),
        })
    }

    fn parse_struct(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<(sym::Sym, ast::Struct)> {
        self.eat(stream)?;
        let id = self.expect_ident()?;

        self.eat(stream)?;
        let mut dn = self.expect_delim()?;
        let parameters = if dn.delim == Paren {
            let ps = self.parse_formal_params(dn.stream)?;
            self.eat(stream)?;
            dn = self.expect_delim()?;
            ps
        } else {
            vec![]
        };

        let fields = if dn.delim == Brace {
            self.parse_struct_fields(dn.stream)?
        } else {
            return Err(self.err(UnexpectedOpenDelim(dn.delim)));
        };

        Ok((id, ast::Struct { parameters, fields }))
    }

    fn parse_formal_params(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<Vec<sym::Sym>> {
        let mut params = Vec::new();

        if stream.not_empty() {
            self.eat(&mut stream)?;
            params.push(self.expect_ident()?);
            while stream.not_empty() {
                self.eat(&mut stream)?;
                self.expect_kind(TokKind::Comma)?;
                self.eat(&mut stream)?;
                params.push(self.expect_ident()?);
            }
        }

        Ok(params)
    }

    fn parse_actual_params(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<Vec<ast::Expr>> {
        let mut params = Vec::new();

        while stream.not_empty() {
            let param_stream = stream.eat_until(&TokKind::Comma);
            if stream.not_empty() {
                self.eat(&mut stream)?; // comma, optional if trailing
            }
            params.push(self.parse_expr(param_stream)?);
        }

        Ok(params)
    }

    fn parse_struct_fields(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<Vec<ast::Field>> {
        let mut fields = Vec::new();

        while stream.not_empty() {
            let field_stream = stream.eat_until(&TokKind::Comma);
            if stream.not_empty() {
                self.eat(&mut stream)?; // comma, trailing is optional
            }

            fields.push(self.parse_struct_field(field_stream)?);
        }

        Ok(fields)
    }

    fn parse_struct_field(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::Field> {
        let ty = self.parse_field_type(&mut stream)?;
        let id = self.parse_field_ident(&mut stream)?;
        let constraint = match stream.peek() {
            Some(TokTree::Token(token)) if token.kind == TokKind::Tilde => {
                self.eat(&mut stream)?; // tilde
                Some(self.parse_field_constraint(stream)?)
            }
            Some(_) => {
                self.eat(&mut stream)?;
                return Err(self.err_hint(
                    Unexpected,
                    "expected constraint or comma after field declaration",
                ));
            }
            _ => None,
        };

        Ok(ast::Field {
            start: ast::Addr::Relative(ast::Expr::Int(0)),
            ty,
            id,
            constraint,
        })
    }

    fn parse_field_type(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<ast::FieldType> {
        match stream.peek() {
            Some(TokTree::Token(Token {
                kind: TokKind::Ident(_),
                ..
            })) => {
                self.eat(stream)?;
                let id = self.expect_ident()?;
                let params = match stream.peek() {
                    Some(TokTree::Delim(dn)) if dn.delim == Paren => {
                        self.eat(stream)?;
                        let dn = self.expect_delim()?;
                        self.parse_actual_params(dn.stream)?
                    }
                    _ => vec![],
                };

                let fty = match (self.symtab.name(id), params.as_slice()) {
                    ("u8", []) => ast::U8,
                    ("s8", []) => ast::S8,
                    ("u16", []) => ast::U16,
                    ("s16", []) => ast::S16,
                    ("u32", []) => ast::U32,
                    ("s32", []) => ast::S32,
                    ("u64", []) => ast::U64,
                    ("s64", []) => ast::S64,
                    ("u128", []) => ast::U64,
                    ("s128", []) => ast::S64,
                    ("f32", []) => ast::F32,
                    ("f64", []) => ast::F64,
                    ("signed", [len]) => ast::FieldType::Prim(
                        ast::PrimType::Signed(len.clone()),
                    ),
                    ("unsigned", [len]) => ast::FieldType::Prim(
                        ast::PrimType::Unsigned(len.clone()),
                    ),
                    ("float", [ex, mt]) => ast::FieldType::Prim(
                        ast::PrimType::Float(ex.clone(), mt.clone()),
                    ),
                    ("bitvec", [len]) => ast::FieldType::Prim(
                        ast::PrimType::BitVec(len.clone()),
                    ),
                    _ => ast::FieldType::Struct(id, params),
                };

                Ok(fty)
            }
            Some(TokTree::Delim(dn)) if dn.delim == Bracket => {
                self.eat(stream)?;
                let dn = self.expect_delim()?;
                self.parse_array_type(dn.stream)
            }
            _ => Err(self.err(Unexpected)),
        }
    }

    // array ::- [<field_type>; <array_size>]
    // array_size ::- ? | * | + | <expr> | {<expr,} | {<expr>, <expr>} | <null>
    fn parse_array_type(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::FieldType> {
        let mut type_stream = stream.eat_until(&TokKind::SemiColon);
        let element_type = self.parse_field_type(&mut type_stream)?;
        if stream.not_empty() {
            self.eat(&mut stream)?; // semicolon, optional if no specified size
        }
        self.assert_eof(&type_stream, "unexpected token after array type");

        let arr_size = match stream.peek_all()[..] {
            [&TokTree::Token(Token {
                kind: TokKind::Question,
                ..
            })] => {
                ast::ArraySize::Within(ast::Expr::Int(0), ast::Expr::Int(1))
            }
            [&TokTree::Token(Token {
                kind: TokKind::Plus,
                ..
            })] => ast::ArraySize::AtLeast(ast::Expr::Int(1)),
            [&TokTree::Token(Token {
                kind: TokKind::Star,
                ..
            })]
            | [] => ast::ArraySize::AtLeast(ast::Expr::Int(0)),
            _ => {
                match stream.peek() {
                    Some(TokTree::Delim(dn)) if dn.delim == Brace => {
                        self.eat(&mut stream)?;
                        let dn = self.expect_delim()?;
                        let mut bounds_stream = dn.stream;

                        let lb_stream =
                            bounds_stream.eat_until(&TokKind::Comma);
                        let lower_bound = self.parse_expr(lb_stream)?;
                        self.eat(&mut stream)?; // comma

                        if bounds_stream.not_empty() {
                            ast::ArraySize::AtLeast(lower_bound)
                        } else {
                            let upper_bound =
                                self.parse_expr(bounds_stream)?;
                            ast::ArraySize::Within(lower_bound, upper_bound)
                        }
                    }
                    _ => {
                        let size = self.parse_expr(stream)?;
                        ast::ArraySize::Exactly(size)
                    }
                }
            }
        };

        Ok(ast::FieldType::Array(Box::new(element_type), arr_size))
    }

    fn parse_field_ident(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<Option<sym::Sym>> {
        match stream.peek() {
            Some(TokTree::Token(Token {
                kind: TokKind::Ident(_),
                ..
            })) => {
                self.eat(stream)?;
                let id = self.expect_ident()?;
                Ok(Some(id))
            }
            _ => Ok(None),
        }
    }

    fn parse_field_constraint(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::Constraint> {
        unimplemented!()
    }

    fn parse_expr(&mut self, mut stream: TokenStream) -> PResult<ast::Expr> {
        let expr = self.parse_expr_fix(&mut stream, 0)?;
        self.assert_eof(&stream, "unexpected token after expression");
        Ok(expr)
    }

    fn parse_expr_fix(
        &mut self,
        stream: &mut TokenStream,
        min_fixity: u8,
    ) -> PResult<ast::Expr> {
        self.eat(stream)?;
        let mut lhs = match self.tree.take() {
            TokTree::Token(token) => match token.kind {
                TokKind::Literal(LitKind::Int(val)) => ast::Expr::Int(val),
                TokKind::Ident(id) => {
                    let mut ids = vec![id];
                    while stream.not_empty() {
                        match stream.peek().unwrap() {
                            TokTree::Token(token)
                                if token.kind == TokKind::Dot =>
                            {
                                self.assert_spacing(Joint, "member access dot may not be preceded by space");
                                self.eat(stream)?; // dot
                                self.assert_spacing(Joint, "member access dot may not be followed by space");
                                self.eat(stream)?;
                                let id = self.expect_ident()?;
                                ids.push(id);
                            }
                            _ => break,
                        }
                    }
                    ast::Expr::Ident(ids)
                }
                TokKind::Minus => {
                    let op = ast::UnOpKind::Neg;
                    let expr = self.parse_expr_fix(stream, op.fixity())?;
                    ast::Expr::Unary(Box::new(ast::UnOp {
                        expr,
                        kind: ast::UnOpKind::Neg,
                    }))
                }
                _ => {
                    return Err(self.err_hint(
                        UnexpectedToken(token.kind),
                        "expected literal, identifier or unary op on lhs",
                    ))
                }
            },
            TokTree::Delim(dn) if dn.delim == Paren => {
                self.parse_expr(dn.stream)?
            }
            TokTree::Delim(dn) => {
                return Err(self.err_hint(
                    UnexpectedOpenDelim(dn.delim),
                    "expression may not contain delimiters except parenthesis",
                ));
            }
        };

        while stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(token) => {
                    let kind = match token.kind {
                        TokKind::Plus => ast::BinOpKind::Add,
                        TokKind::Minus => ast::BinOpKind::Sub,
                        TokKind::Star => ast::BinOpKind::Mul,
                        TokKind::Slash => ast::BinOpKind::Div,
                        TokKind::Percentage => ast::BinOpKind::Rem,
                        TokKind::Ampersand => ast::BinOpKind::BitAnd,
                        TokKind::Pipe => ast::BinOpKind::BitOr,
                        TokKind::Lt2 => ast::BinOpKind::Shl,
                        TokKind::Gt2 => ast::BinOpKind::Shr,
                        _ => {
                            return Err(self.err_hint(
                                UnexpectedToken(token.kind.clone()),
                                "expected binary operator",
                            ));
                        }
                    };

                    let (lfix, rfix) = kind.fixity();
                    if lfix < min_fixity {
                        break;
                    }

                    self.eat(stream)?; // bin op
                    let rhs = self.parse_expr_fix(stream, rfix)?;
                    let binop = ast::BinOp { lhs, rhs, kind };
                    lhs = ast::Expr::Binary(Box::new(binop));
                }
                _ => {
                    return Err(
                        self.err_hint(Unexpected, "expected binary operator")
                    );
                }
            }
        }

        Ok(lhs)
    }

    fn err(&self, kind: PErrorKind) -> PError {
        PError {
            kind,
            pos: self.pos,
            hint: None,
        }
    }

    fn err_hint(&self, kind: PErrorKind, hint: &'static str) -> PError {
        PError {
            kind,
            pos: self.pos,
            hint: Some(hint),
        }
    }

    fn consume(self) -> (sym::SymbolTable, Vec<PError>) {
        (self.symtab, self.errors)
    }
}
