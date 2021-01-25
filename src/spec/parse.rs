use crate::{
    AddrBase, BuiltIn, Error, ErrorType, Order, SpannedSym, Sym, SymbolTable,
};

use super::ast;
use super::lex::Delim::{Brace, Bracket, Paren};
use super::lex::{
    Attr, BinConstr, Delim, DelimNode, Keyword, LitKind, Symbol, TokKind,
    TokTree, Token, TokenStream,
};
use super::Span;

use self::PErrorKind::*;

#[derive(Clone, Debug)]
enum PErrorKind {
    Unexpected,
    UnexpectedToken(TokKind),
    UnexpectedSymbol(Symbol),
    UnexpectedKeyword(Keyword),
    UnexpectedOpenDelim(Delim),
    UnexpectedCloseDelimOrEof,
}

#[derive(Clone, Debug)]
struct PError {
    kind: PErrorKind,
    span: Span,
    hint: Option<&'static str>,
}

type PResult<T> = Result<T, PError>;

impl From<PError> for Error {
    fn from(p: PError) -> Self {
        let desc = match p.kind {
            Unexpected => "unexpected token or delimiter".to_string(),
            UnexpectedToken(token) => {
                format!("unexpected token -- {:?}", token)
            }
            UnexpectedSymbol(s) => {
                format!("unexpected symbol -- {:?}", s)
            }
            UnexpectedKeyword(keyword) => {
                format!("unexpected keyword -- {:?}", keyword)
            }
            UnexpectedOpenDelim(delim) => {
                format!("unexpected opening delimiter -- {:?}", delim)
            }
            UnexpectedCloseDelimOrEof => {
                "unexpected closing delimiter or end of file".to_string()
            }
        };
        let hint = p.hint;

        Error {
            span: p.span,
            backtrace: Vec::new(),
            desc,
            hint,
            ty: ErrorType::Parsing,
        }
    }
}

pub fn parse_file_spec(
    symtab: SymbolTable,
    tokens: TokenStream,
) -> (Result<ast::Struct, Error>, SymbolTable, Vec<Error>) {
    let mut parser = Parser::new(symtab);

    let file_spec = parser.parse_inner_struct(tokens);
    let (symtab, errors) = parser.consume();

    (
        file_spec.map_err(|pe| pe.into()),
        symtab,
        errors.into_iter().map(|pe| pe.into()).collect(),
    )
}

struct Parser {
    symtab: SymbolTable,

    tree: TokTree,

    span: Span,
    prev_span: Span,
    delim_span: Span,

    errors: Vec<PError>,
}

impl Parser {
    fn new(symtab: SymbolTable) -> Self {
        Parser {
            symtab,
            tree: TokTree::Token(Token::dummy()),
            span: Span(0, 0),
            prev_span: Span(0, 0),
            delim_span: Span(0, 0),
            errors: Vec::new(),
        }
    }

    fn eat(&mut self, stream: &mut TokenStream) -> PResult<()> {
        self.prev_span = self.span;

        if stream.not_empty() {
            let tree = stream.eat().unwrap();

            self.span = tree.span();
            self.tree = tree;

            Ok(())
        } else {
            self.span = Span(self.delim_span.1 - 1, self.delim_span.1);
            Err(self.err(UnexpectedCloseDelimOrEof))
        }
    }

    fn expect_token(&mut self) -> PResult<Token> {
        match self.tree.take() {
            TokTree::Token(token) => Ok(token),
            TokTree::Delim(dn) => Err(self.err(UnexpectedOpenDelim(dn.delim))),
        }
    }

    fn expect_ident(&mut self) -> PResult<Sym> {
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

    fn assert_symbol(&mut self, symbol: Symbol) -> PResult<()> {
        let token = self.expect_token()?;

        match token.kind {
            TokKind::Symbol(s) if s == symbol => Ok(()),
            k => Err(self.err(UnexpectedToken(k))),
        }
    }

    fn assert_eof(&mut self, stream: &TokenStream, hint: &'static str) {
        if let Some(tree) = stream.peek() {
            self.errors.push(PError {
                kind: Unexpected,
                span: tree.span(),
                hint: Some(hint),
            });
        }
    }

    // parse struct, including identifier and formal parameters
    fn parse_struct(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<(Sym, ast::Struct)> {
        self.eat(stream)?; // id
        let id = self.expect_ident()?;

        self.eat(stream)?;
        let mut dn = self.expect_delim()?;
        let formal_params = if dn.delim == Paren {
            let ps = self.parse_formal_params(dn.stream)?;
            self.eat(stream)?;
            dn = self.expect_delim()?;
            ps
        } else {
            vec![]
        };

        if dn.delim == Brace {
            let mut spec = self.parse_inner_struct(dn.stream)?;
            spec.formal_params = formal_params;
            Ok((id, spec))
        } else {
            Err(self.err_hint(
                UnexpectedOpenDelim(dn.delim),
                "expected braces after struct id",
            ))
        }
    }

    fn parse_formal_params(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<Vec<Sym>> {
        let mut params = Vec::new();

        if stream.not_empty() {
            self.eat(&mut stream)?;
            params.push(self.expect_ident()?);
            while stream.not_empty() {
                self.eat(&mut stream)?;
                self.assert_symbol(Symbol::Comma)?;
                self.eat(&mut stream)?;
                params.push(self.expect_ident()?);
            }
        }

        Ok(params)
    }

    fn parse_inner_struct(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::Struct> {
        let mut structs = Vec::new();
        let mut constants = Vec::new();
        while stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(Token {
                    kind: TokKind::Keyword(Keyword::Def),
                    ..
                }) => {
                    self.eat(&mut stream)?; // def
                    let (id, st) = self.parse_struct(&mut stream)?;
                    structs.push((id, st));
                }
                TokTree::Token(Token {
                    kind: TokKind::Keyword(Keyword::Const),
                    ..
                }) => {
                    self.eat(&mut stream)?; // const
                    let (id, expr) = self.parse_assign(&mut stream)?;
                    constants.push((id, expr));
                }
                _ => break,
            }
        }

        let block = self.parse_block(stream)?;

        Ok(ast::Struct {
            formal_params: vec![],
            structs,
            constants,
            block,
        })
    }

    fn parse_block(&mut self, mut stream: TokenStream) -> PResult<ast::Block> {
        let mut stmts = Vec::new();

        while stream.not_empty() {
            if let TokTree::Token(Token {
                kind: TokKind::Keyword(kw),
                ..
            }) = stream.peek().unwrap()
            {
                let kw = *kw;
                self.eat(&mut stream)?; // keyword
                match kw {
                    Keyword::Let => {
                        let (id, expr) = self.parse_assign(&mut stream)?;
                        stmts.push(ast::Stmt::Let(id, expr));
                    }
                    Keyword::If => {
                        stmts.push(ast::Stmt::If(self.parse_if(&mut stream)?));
                    }
                    Keyword::Constrain => {
                        self.eat(&mut stream)?; // block
                        let dn = self.expect_delim()?;
                        stmts.push(ast::Stmt::Constrain(
                            self.parse_expr_list(dn.stream)?,
                        ));
                    }
                    Keyword::Debug => {
                        self.eat(&mut stream)?; // block
                        let dn = self.expect_delim()?;
                        stmts.push(ast::Stmt::Debug(
                            self.parse_expr_list(dn.stream)?,
                        ));
                    }
                    Keyword::Def | Keyword::Const => {
                        return Err(self.err_hint(
                            UnexpectedKeyword(kw),
                            "may only be defined in struct header",
                        ));
                    }
                    _ => {
                        return Err(self.err(UnexpectedKeyword(kw)));
                    }
                }
            } else {
                let field_stream = stream.eat_until(Symbol::Comma);
                if stream.not_empty() {
                    self.eat(&mut stream)?; // comma, trailing is optional
                }

                if field_stream.not_empty() {
                    stmts.push(ast::Stmt::Field(
                        self.parse_struct_field(field_stream)?,
                    ));
                }
            }
        }

        Ok(stmts)
    }

    fn parse_assign(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<(Sym, ast::Expr)> {
        self.eat(stream)?; // id
        let id = self.expect_ident()?;
        self.eat(stream)?; // =
        self.assert_symbol(Symbol::Eq)?;
        let expr_stream = stream.eat_until(Symbol::Comma);
        if stream.not_empty() {
            self.eat(stream)?; // comma
        }
        let expr = self.parse_expr(expr_stream)?;
        Ok((id, expr))
    }

    fn parse_if(&mut self, stream: &mut TokenStream) -> PResult<ast::IfStmt> {
        self.eat(stream)?; // cond delim
        let dn = self.expect_delim()?;
        let cond = if dn.delim == Paren {
            self.parse_expr(dn.stream)?
        } else {
            return Err(self.err_hint(
                UnexpectedOpenDelim(dn.delim),
                "expected condition after if",
            ));
        };

        self.eat(stream)?; // if body delim
        let dn = self.expect_delim()?;
        let if_body = if dn.delim == Brace {
            self.parse_block(dn.stream)?
        } else {
            return Err(self.err_hint(
                UnexpectedOpenDelim(dn.delim),
                "expected body after if",
            ));
        };

        let mut elseifs = Vec::new();
        let else_body = loop {
            if let Some(TokTree::Token(Token {
                kind: TokKind::Keyword(Keyword::Else),
                ..
            })) = stream.peek()
            {
                self.eat(stream)?; // else
                let cond_opt = if let Some(TokTree::Token(Token {
                    kind: TokKind::Keyword(Keyword::If),
                    ..
                })) = stream.peek()
                {
                    // else if
                    self.eat(stream)?; // if
                    self.eat(stream)?; // condition delim
                    let dn = self.expect_delim()?;
                    let cond = if dn.delim == Paren {
                        self.parse_expr(dn.stream)?
                    } else {
                        return Err(self.err_hint(
                            UnexpectedOpenDelim(dn.delim),
                            "expected condition after else if",
                        ));
                    };
                    Some(cond)
                } else {
                    // else (has no condition)
                    None
                };

                self.eat(stream)?; // body delim
                let dn = self.expect_delim()?;
                let body = if dn.delim == Brace {
                    self.parse_block(dn.stream)?
                } else {
                    return Err(self.err_hint(
                        UnexpectedOpenDelim(dn.delim),
                        "expected body after if/else",
                    ));
                };

                if let Some(cond) = cond_opt {
                    elseifs.push((cond, body));
                } else {
                    break body;
                }
            } else {
                // other, does not belong to if statement
                break vec![];
            }
        };

        Ok(ast::IfStmt {
            cond,
            if_body,
            elseifs,
            else_body,
        })
    }

    fn parse_struct_field(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::Field> {
        let span = stream.span();
        let ty = self.parse_field_type(&mut stream)?;
        let id = match stream.peek() {
            Some(TokTree::Token(Token {
                kind: TokKind::Ident(_),
                ..
            })) => {
                self.eat(&mut stream)?;
                let id = self.expect_ident()?;
                Some(id)
            }
            _ => None,
        };
        let hidden = id.map_or(false, |sym| {
            self.symtab.name(sym).unwrap().starts_with('_')
        });
        self.assert_eof(&stream, "expected comma after field declaration");

        Ok(ast::Field {
            ty,
            id,
            span,
            hidden,
        })
    }

    fn parse_attrs(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<Vec<(Attr, Vec<TokenStream>)>> {
        let mut attributes = Vec::new();

        while let Some(TokTree::Token(Token {
            kind: TokKind::Attr(attr),
            ..
        })) = stream.peek()
        {
            let attr = attr.clone();
            self.eat(stream)?; // attr name
            let argstream = if let Some(TokTree::Delim(DelimNode {
                delim: Paren,
                ..
            })) = stream.peek()
            {
                self.eat(stream)?; // arguments
                self.expect_delim()?.stream.split_on(Symbol::Comma)
            } else {
                vec![]
            };
            attributes.push((attr, argstream));
        }

        Ok(attributes)
    }

    fn parse_field_type(
        &mut self,
        stream: &mut TokenStream,
    ) -> PResult<ast::FieldType> {
        let mut byte_order = Order::LittleEndian;
        let mut alignment = ast::Alignment {
            expr: None,
            bitwise: false,
        };
        let mut loc = ast::Location {
            expr: None,
            base: AddrBase::Absolute,
            bitwise: false,
        };
        let mut constraints = Vec::new();
        let attrs = self.parse_attrs(stream)?;
        for (attr, mut args) in attrs {
            match attr {
                Attr::Align(bitwise) => {
                    alignment = ast::Alignment {
                        expr: Some(self.parse_expr(args.remove(0))?),
                        bitwise,
                    }
                }
                Attr::Location { base, bitwise } => {
                    let expr = Some(self.parse_expr(args.remove(0))?);
                    loc = ast::Location {
                        expr,
                        base,
                        bitwise,
                    };
                }
                Attr::Order => {
                    let mut arg0 = args.remove(0);
                    self.eat(&mut arg0)?;
                    let ident = self.expect_ident()?;
                    self.assert_eof(&arg0, "order takes only le or be");
                    byte_order = match self.symtab.name(ident).unwrap() {
                        "le" => Order::LittleEndian,
                        "be" => Order::BigEndian,
                        _ => {
                            return Err(
                                self.err_hint(Unexpected, "expected le or be")
                            )
                        }
                    }
                }
                Attr::Constraint => {
                    constraints.push(ast::Constraint::Generic(
                        self.parse_expr(args.remove(0))?,
                    ));
                }
                Attr::Zero(z) => {
                    constraints.push(ast::Constraint::Zero(z));
                }
                Attr::BinConstr(rel) => {
                    let binop = match rel {
                        BinConstr::Eq => ast::BinOp::Eq,
                        BinConstr::Neq => ast::BinOp::Neq,
                        BinConstr::Lt => ast::BinOp::Lt,
                        BinConstr::Gt => ast::BinOp::Gt,
                        BinConstr::Leq => ast::BinOp::Leq,
                        BinConstr::Geq => ast::BinOp::Geq,
                    };
                    constraints.push(ast::Constraint::Binary(
                        binop,
                        self.parse_expr(args.remove(0))?,
                    ));
                }
            }

            if !args.is_empty() {
                return Err(self
                    .err_hint(Unexpected, "too many arguments to attribute"));
            }
        }

        self.eat(stream)?;
        let kind = match self.tree.take() {
            TokTree::Token(Token {
                kind: TokKind::Ident(id),
                ..
            }) => {
                let ssym = SpannedSym {
                    span: self.span,
                    sym: id,
                };
                let params = match stream.peek() {
                    Some(TokTree::Delim(dn)) if dn.delim == Paren => {
                        self.eat(stream)?;
                        let dn = self.expect_delim()?;
                        self.parse_expr_list(dn.stream)?
                    }
                    _ => vec![],
                };

                let parts: Vec<_> =
                    self.symtab.name(id).unwrap().split('_').collect();
                let ident = parts[0];
                let ord = parts.get(1);
                let kind = if let Some(kind) =
                    alias_field_kind(ident, params.as_slice())
                {
                    if parts.len() == 1 {
                        kind
                    } else if parts.len() == 2
                        && (ord == Some(&"le") || ord == Some(&"be"))
                    {
                        byte_order = match *ord.unwrap() {
                            "le" => Order::LittleEndian,
                            "be" => Order::BigEndian,
                            _ => unreachable!(),
                        };
                        kind
                    } else {
                        ast::FieldKind::Struct(ssym, params)
                    }
                } else {
                    ast::FieldKind::Struct(ssym, params)
                };

                kind
            }
            TokTree::Delim(dn) if dn.delim == Bracket => {
                ast::FieldKind::Array(self.parse_array_type(dn.stream)?)
            }
            TokTree::Delim(dn) if dn.delim == Brace => {
                ast::FieldKind::Block(self.parse_block(dn.stream)?)
            }
            _ => return Err(self.err_hint(Unexpected, "expected field type")),
        };

        Ok(ast::FieldType {
            kind,
            byte_order,
            loc,
            alignment,
            constraints,
        })
    }

    fn parse_expr_list(
        &mut self,
        stream: TokenStream,
    ) -> PResult<Vec<ast::Expr>> {
        stream
            .split_on(Symbol::Comma)
            .into_iter()
            .map(|s| self.parse_expr(s))
            .collect()
    }

    // array ::- std_array | for_array
    fn parse_array_type(
        &mut self,
        stream: TokenStream,
    ) -> PResult<ast::Array> {
        Ok(match stream.peek() {
            Some(TokTree::Token(Token {
                kind: TokKind::Keyword(Keyword::For),
                ..
            })) => ast::Array::For(self.parse_for_array(stream)?),
            _ => ast::Array::Std(self.parse_std_array(stream)?),
        })
    }

    // std_array ::- [<field_type>; <array_size>]
    // array_size ::- ? | * | + | <expr> | {<expr,} | {<expr>, <expr>} | <null>
    fn parse_std_array(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::StdArray> {
        let mut type_stream = stream.eat_until(Symbol::SemiColon);
        if stream.not_empty() {
            self.eat(&mut stream)?; // semicolon, optional if no specified size
        }
        let element_type = self.parse_field_type(&mut type_stream)?;
        self.assert_eof(
            &type_stream,
            "expected semicolon or closing bracket after array type",
        );

        let arr_size = if stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(Token {
                    kind: TokKind::Symbol(s),
                    ..
                }) if *s == Symbol::Question
                    || *s == Symbol::Plus
                    || *s == Symbol::Star =>
                {
                    self.eat(&mut stream)?;
                    let t = self.expect_token()?;
                    self.assert_eof(
                        &stream,
                        "expected no more tokens after array size",
                    );
                    let span = t.span;
                    if let TokKind::Symbol(s) = t.kind {
                        match s {
                            Symbol::Question => ast::ArraySize::Within(
                                ast::Expr {
                                    kind: ast::ExprKind::Literal(
                                        LitKind::Int(0),
                                    ),
                                    span,
                                },
                                ast::Expr {
                                    kind: ast::ExprKind::Literal(
                                        LitKind::Int(1),
                                    ),
                                    span,
                                },
                            ),
                            Symbol::Plus => {
                                ast::ArraySize::AtLeast(ast::Expr {
                                    kind: ast::ExprKind::Literal(
                                        LitKind::Int(1),
                                    ),
                                    span,
                                })
                            }
                            Symbol::Star => {
                                ast::ArraySize::AtLeast(ast::Expr {
                                    kind: ast::ExprKind::Literal(
                                        LitKind::Int(0),
                                    ),
                                    span,
                                })
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        unreachable!();
                    }
                }
                TokTree::Delim(dn) if dn.delim == Brace => {
                    self.eat(&mut stream)?;
                    let dn = self.expect_delim()?;
                    let mut bounds_stream = dn.stream;

                    let lb_stream = bounds_stream.eat_until(Symbol::Comma);
                    let lower_bound = self.parse_expr(lb_stream)?;
                    self.eat(&mut bounds_stream)?; // comma

                    if bounds_stream.not_empty() {
                        let upper_bound = self.parse_expr(bounds_stream)?;
                        ast::ArraySize::Within(lower_bound, upper_bound)
                    } else {
                        ast::ArraySize::AtLeast(lower_bound)
                    }
                }
                _ => {
                    let size = self.parse_expr(stream)?;
                    ast::ArraySize::Exactly(size)
                }
            }
        } else {
            ast::ArraySize::AtLeast(ast::Expr {
                kind: ast::ExprKind::Literal(LitKind::Int(0)),
                span: self.span,
            })
        };

        Ok(ast::StdArray {
            ty: Box::new(element_type),
            size: arr_size,
        })
    }

    // for_array ::- [for <sym> in <sas> <block>]
    fn parse_for_array(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<ast::ForArray> {
        self.eat(&mut stream)?; // for kw
        self.eat(&mut stream)?; // elem sym
        let elem = self.expect_ident()?;

        self.eat(&mut stream)?; // in keword
        let kw_tok = self.expect_token()?;
        match kw_tok.kind {
            TokKind::Keyword(Keyword::In) => {}
            k => return Err(self.err(UnexpectedToken(k))),
        };

        let arr = self.parse_expr_fix(&mut stream, 100)?;

        let ty = Box::new(self.parse_field_type(&mut stream)?);
        self.assert_eof(&stream, "unexpected junk after for loop");

        Ok(ast::ForArray { elem, arr, ty })
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
        let mut lhs_span = self.tree.span();
        let mut lhs_kind = match self.tree.take() {
            TokTree::Token(token) => match token.kind {
                TokKind::Symbol(Symbol::Dot) => ast::ExprKind::Variable(
                    self.symtab.builtin(BuiltIn::IdentSelf),
                ),
                TokKind::Literal(litkind) => ast::ExprKind::Literal(litkind),
                TokKind::Ident(sym) => ast::ExprKind::Variable(sym),
                TokKind::Symbol(s) => {
                    let op = match s {
                        Symbol::Minus => ast::UnOp::Neg,
                        Symbol::Exclamation => ast::UnOp::Not,
                        _ => {
                            return Err(self.err_hint(
                                UnexpectedSymbol(s),
                                "expected literal, identifier or unary op",
                            ));
                        }
                    };
                    let expr = self.parse_expr_fix(stream, op.fixity())?;
                    ast::ExprKind::Unary(op, Box::new(expr))
                }
                k => {
                    return Err(self.err_hint(
                        UnexpectedToken(k),
                        "expected literal, identifier or unary op",
                    ))
                }
            },
            TokTree::Delim(dn) if dn.delim == Paren => {
                self.parse_expr(dn.stream)?.kind
            }
            TokTree::Delim(dn) => {
                return Err(self.err_hint(
                    UnexpectedOpenDelim(dn.delim),
                    "expression may not contain delimiters except parenthesis",
                ));
            }
        };
        lhs_span.1 = self.tree.span().1;

        while stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(tok)
                    if matches!(
                        tok.kind,
                        TokKind::Symbol(Symbol::Dot) | TokKind::Ident(_)
                    ) =>
                {
                    if matches!(tok.kind, TokKind::Symbol(Symbol::Dot)) {
                        self.eat(stream)?; // dot
                    }
                    self.eat(stream)?; // ident
                    let sym = self.expect_ident()?;
                    let st = ast::Expr {
                        kind: lhs_kind,
                        span: lhs_span,
                    };
                    let ssym = SpannedSym {
                        span: self.span,
                        sym,
                    };
                    lhs_kind = ast::ExprKind::Member(Box::new(st), ssym);
                }
                TokTree::Delim(dn) if dn.delim == Bracket => {
                    self.eat(stream)?; // bracket delim
                    let dn = self.expect_delim()?;
                    let index = self.parse_expr(dn.stream)?;
                    let arr = ast::Expr {
                        kind: lhs_kind,
                        span: lhs_span,
                    };
                    lhs_kind =
                        ast::ExprKind::Index(Box::new(arr), Box::new(index));
                }
                TokTree::Token(Token {
                    kind: TokKind::Symbol(s),
                    ..
                }) if *s == Symbol::Apostrophe => {
                    self.eat(stream)?; // apostrophe
                    self.eat(stream)?; // id
                    let prop = self.expect_ident()?;
                    let expr = ast::Expr {
                        kind: lhs_kind,
                        span: lhs_span,
                    };
                    let ssym = SpannedSym {
                        span: self.span,
                        sym: prop,
                    };
                    lhs_kind = ast::ExprKind::Property(Box::new(expr), ssym);
                }
                TokTree::Token(Token {
                    kind: TokKind::Symbol(s),
                    ..
                }) => {
                    let op = match s {
                        Symbol::Plus => ast::BinOp::Add,
                        Symbol::Minus => ast::BinOp::Sub,
                        Symbol::Star => ast::BinOp::Mul,
                        Symbol::Slash => ast::BinOp::Div,
                        Symbol::Percentage => ast::BinOp::Rem,
                        Symbol::Ampersand => ast::BinOp::BitAnd,
                        Symbol::Caret => ast::BinOp::BitXor,
                        Symbol::Pipe => ast::BinOp::BitOr,
                        Symbol::Eq2 => ast::BinOp::Eq,
                        Symbol::Neq => ast::BinOp::Neq,
                        Symbol::Ampersand2 => ast::BinOp::And,
                        Symbol::Pipe2 => ast::BinOp::Or,
                        Symbol::Lt => ast::BinOp::Lt,
                        Symbol::Gt => ast::BinOp::Gt,
                        Symbol::Leq => ast::BinOp::Leq,
                        Symbol::Geq => ast::BinOp::Geq,
                        Symbol::Lt2 => ast::BinOp::Shl,
                        Symbol::Gt2 => ast::BinOp::Shr,
                        _ => break,
                    };

                    let (lfix, rfix) = op.fixity();
                    if lfix < min_fixity {
                        break;
                    }

                    self.eat(stream)?; // bin op
                    let rhs = self.parse_expr_fix(stream, rfix)?;
                    let lhs = ast::Expr {
                        kind: lhs_kind,
                        span: lhs_span,
                    };
                    lhs_kind = ast::ExprKind::Binary(
                        op,
                        Box::new(lhs),
                        Box::new(rhs),
                    );
                }
                _ => break,
            }
            lhs_span.1 = self.span.1;
        }

        Ok(ast::Expr {
            kind: lhs_kind,
            span: lhs_span,
        })
    }

    fn err(&self, kind: PErrorKind) -> PError {
        PError {
            kind,
            span: self.span,
            hint: None,
        }
    }

    fn err_hint(&self, kind: PErrorKind, hint: &'static str) -> PError {
        PError {
            kind,
            span: self.span,
            hint: Some(hint),
        }
    }

    fn consume(self) -> (SymbolTable, Vec<PError>) {
        (self.symtab, self.errors)
    }
}

fn alias_field_kind(
    alias: &str,
    params: &[ast::Expr],
) -> Option<ast::FieldKind> {
    let kind = match (alias, params) {
        ("char", []) => ast::FieldKind::Prim(ast::PrimType::Char),
        ("u8", []) => ast::FieldKind::Prim(ast::PrimType::U8),
        ("i8", []) => ast::FieldKind::Prim(ast::PrimType::I8),
        ("u16", []) => ast::FieldKind::Prim(ast::PrimType::U16),
        ("i16", []) => ast::FieldKind::Prim(ast::PrimType::I16),
        ("u32", []) => ast::FieldKind::Prim(ast::PrimType::U32),
        ("i32", []) => ast::FieldKind::Prim(ast::PrimType::I32),
        ("u64", []) => ast::FieldKind::Prim(ast::PrimType::U64),
        ("i64", []) => ast::FieldKind::Prim(ast::PrimType::I64),
        ("f32", []) => ast::FieldKind::Prim(ast::PrimType::F32),
        ("f64", []) => ast::FieldKind::Prim(ast::PrimType::F64),
        ("bitvec", [len]) => {
            ast::FieldKind::Prim(ast::PrimType::BitVec(len.clone()))
        }
        _ => return None,
    };

    Some(kind)
}
