use std::collections::HashMap;

use crate::ast;
use crate::lex::Delim::*;
use crate::lex::{
    Delim, DelimNode, Keyword, Spacing, Span, TokKind, TokTree, Token,
    TokenStream,
};
use crate::sym;

use self::PErrorKind::*;

#[derive(Clone, Debug)]
pub enum PErrorKind {
    Unexpected,
    UnexpectedToken(TokKind),
    UnexpectedOpenDelim(Delim),
    UnexpectedCloseDelimOrEof,
}

#[derive(Clone, Debug)]
pub struct PError {
    pub kind: PErrorKind,
    pub pos: u32,
    pub hint: Option<&'static str>,
}

type PResult<T> = Result<T, PError>;

pub fn parse_file_spec(
    symtab: sym::SymbolTable,
    mut tokens: TokenStream,
) -> (PResult<ast::FileSpecification>, sym::SymbolTable) {
    let mut parser = Parser::new(symtab);

    let file_spec = parser.parse_spec(&mut tokens);
    let symtab = parser.consume();

    (file_spec, symtab)
}

struct Parser {
    symtab: sym::SymbolTable,
    tree: TokTree,
    pos: u32,
    delim_span: Span,
}

impl Parser {
    fn new(symtab: sym::SymbolTable) -> Self {
        Parser {
            symtab,
            tree: TokTree::Token(Token::dummy()),
            pos: 0,
            delim_span: Span(0, 0),
        }
    }

    fn eat(&mut self, stream: &mut TokenStream) -> PResult<Spacing> {
        if stream.not_empty() {
            let (tree, spacing) = stream.eat().unwrap();

            let pos = match &tree {
                TokTree::Token(token) => token.span.0,
                TokTree::Delim(DelimNode { span, .. }) => span.0,
            };

            self.tree = tree;
            self.pos = pos;

            Ok(spacing)
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

    fn expect_eof(&mut self, stream: &TokenStream) -> PResult<()> {
        match stream.peek() {
            Some(_) => {
                Err(self.err_hint(Unexpected, "expected closing delimiter"))
            }
            None => Ok(()),
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
            self.eat(&mut stream)?; // comma
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
                self.eat(&mut stream)?; // colon, trailing is optional
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
            Some(TokTree::Token(Token {
                kind: TokKind::Tilde,
                ..
            })) => {
                self.eat(&mut stream)?; // tilde
                Some(self.parse_field_constraint(stream)?)
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
                    Some(TokTree::Delim(DelimNode {
                        delim: Paren, ..
                    })) => {
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
            Some(TokTree::Delim(DelimNode { delim: Bracket, .. })) => {
                self.eat(stream)?;
                let dn = self.expect_delim()?;
                self.parse_array_type(dn.stream)
            }
            _ => Err(self.err(Unexpected)),
        }
    }

    // array -> [ <field_type> ; <array_size> ]
    // array_size -> ? | * | + | <expr> | {<expr,} | {<expr>, <expr>} | <null>
    fn parse_array_type(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::FieldType> {
        let mut type_stream = stream.eat_until(&TokKind::SemiColon);
        let element_type = self.parse_field_type(&mut type_stream)?;
        if stream.not_empty() {
            self.eat(&mut stream)?; // semicolon, optional if no specified size
        }
        self.expect_eof(&type_stream)?;

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
                    Some(TokTree::Delim(DelimNode {
                        delim: Brace, ..
                    })) => {
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

    fn parse_expr(&mut self, stream: TokenStream) -> PResult<ast::Expr> {
        unimplemented!()
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

    fn consume(self) -> sym::SymbolTable {
        self.symtab
    }
}
