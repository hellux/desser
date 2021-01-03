use std::collections::HashMap;

use crate::{AddrBase, Error, ErrorType, Order, Sym, SymbolTable};

use super::ast;
use super::lex::Delim::{Brace, Bracket, Paren};
use super::lex::{
    Attr, Delim, DelimNode, Keyword, LitKind, TokKind, TokTree, Token,
    TokenStream,
};
use super::Span;

use self::PErrorKind::*;

#[derive(Clone, Debug)]
enum PErrorKind {
    Unexpected,
    UnexpectedToken(TokKind),
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
            Unexpected => format!("unexpected token or delimiter"),
            UnexpectedToken(token) => {
                format!("unexpected token -- {:?}", token)
            }
            UnexpectedKeyword(keyword) => {
                format!("unexpected keyword -- {:?}", keyword)
            }
            UnexpectedOpenDelim(delim) => {
                format!("unexpected opening delimiter -- {:?}", delim)
            }
            UnexpectedCloseDelimOrEof => {
                format!("unexpected closing delimiter or end of file")
            }
        };
        let hint = p.hint;

        Error {
            span: p.span,
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

    let file_spec =
        parser.parse_inner_struct(tokens).map(|(structs, block)| {
            ast::Struct {
                formal_params: vec![],
                structs,
                block,
            }
        });
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

    fn expect_kind(&mut self, kind: TokKind) -> PResult<()> {
        let token = self.expect_token()?;

        match token.kind {
            k if k == kind => Ok(()),
            k => Err(self.err(UnexpectedToken(k))),
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

    fn assert_eof(&mut self, stream: &TokenStream, hint: &'static str) {
        match stream.peek() {
            Some(tree) => self.errors.push(PError {
                kind: Unexpected,
                span: tree.span(),
                hint: Some(hint),
            }),
            None => {}
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
            let (structs, block) = self.parse_inner_struct(dn.stream)?;
            Ok((
                id,
                ast::Struct {
                    formal_params,
                    structs,
                    block,
                },
            ))
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
                self.expect_kind(TokKind::Comma)?;
                self.eat(&mut stream)?;
                params.push(self.expect_ident()?);
            }
        }

        Ok(params)
    }

    fn parse_inner_struct(
        &mut self,
        mut stream: TokenStream,
    ) -> PResult<(HashMap<Sym, ast::Struct>, ast::Block)> {
        let mut structs = HashMap::new();
        while stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(Token {
                    kind: TokKind::Keyword(Keyword::Def),
                    ..
                }) => {
                    self.eat(&mut stream)?; // def
                    let (id, st) = self.parse_struct(&mut stream)?;
                    structs.insert(id, st);
                }
                _ => break,
            }
        }

        let block = self.parse_block(stream)?;

        Ok((structs, block))
    }

    fn parse_block(&mut self, mut stream: TokenStream) -> PResult<ast::Block> {
        let mut stmts = Vec::new();

        while stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(Token {
                    kind: TokKind::Keyword(kw),
                    ..
                }) => {
                    let kw = *kw;
                    self.eat(&mut stream)?; // keyword
                    match kw {
                        Keyword::If => {
                            stmts.push(ast::Stmt::If(
                                self.parse_if(&mut stream)?,
                            ));
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
                        Keyword::Def => {
                            return Err(self.err_hint(
                                UnexpectedKeyword(kw),
                                "structs may only be defined before fields",
                            ));
                        }
                        _ => {
                            return Err(self.err(UnexpectedKeyword(kw)));
                        }
                    }
                }
                _ => {
                    let field_stream = stream.eat_until(&TokKind::Comma);
                    if stream.not_empty() {
                        self.eat(&mut stream)?; // comma, trailing is optional
                    }

                    stmts.push(ast::Stmt::Field(
                        self.parse_struct_field(field_stream)?,
                    ));
                }
            }
        }

        Ok(stmts)
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
        let hidden = if let Some(sym) = id {
            self.symtab.name(sym).starts_with("_")
        } else {
            false
        };
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

        loop {
            match stream.peek() {
                Some(TokTree::Token(Token {
                    kind: TokKind::Attr(attr),
                    ..
                })) => {
                    let attr = *attr;
                    self.eat(stream)?; // attr name
                    self.eat(stream)?; // arguments
                    let dn = self.expect_delim()?;
                    if dn.delim == Paren {
                        attributes
                            .push((attr, dn.stream.split_on(&TokKind::Comma)));
                    } else {
                        return Err(self.err_hint(
                            Unexpected,
                            "expected parenthesis with attribute arguments",
                        ));
                    }
                }
                _ => break,
            }
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
        let mut constraint = None;
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
                Attr::Constraint => {
                    constraint = Some(self.parse_expr(args.remove(0))?);
                }
                Attr::Order => {
                    let mut arg0 = args.remove(0);
                    self.eat(&mut arg0)?;
                    let ident = self.expect_ident()?;
                    self.assert_eof(&arg0, "order takes only le or be");
                    byte_order = match self.symtab.name(ident) {
                        "le" => Order::LittleEndian,
                        "be" => Order::BigEndian,
                        _ => {
                            return Err(
                                self.err_hint(Unexpected, "expected le or be")
                            )
                        }
                    }
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
                let params = match stream.peek() {
                    Some(TokTree::Delim(dn)) if dn.delim == Paren => {
                        self.eat(stream)?;
                        let dn = self.expect_delim()?;
                        self.parse_expr_list(dn.stream)?
                    }
                    _ => vec![],
                };

                let parts: Vec<_> = self.symtab.name(id).split('_').collect();
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
                        byte_order = match ord.unwrap() {
                            &"le" => Order::LittleEndian,
                            &"be" => Order::BigEndian,
                            _ => unreachable!(),
                        };
                        kind
                    } else {
                        ast::FieldKind::Struct(id, params)
                    }
                } else {
                    ast::FieldKind::Struct(id, params)
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
            constraint,
        })
    }

    fn parse_expr_list(
        &mut self,
        stream: TokenStream,
    ) -> PResult<Vec<ast::Expr>> {
        stream
            .split_on(&TokKind::Comma)
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
        let mut type_stream = stream.eat_until(&TokKind::SemiColon);
        let element_type = self.parse_field_type(&mut type_stream)?;
        if stream.not_empty() {
            self.eat(&mut stream)?; // semicolon, optional if no specified size
        }
        self.assert_eof(
            &type_stream,
            "expected semicolon or closing bracket after array type",
        );

        let arr_size = if stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(Token { kind: k, .. })
                    if k == &TokKind::Question
                        || k == &TokKind::Plus
                        || k == &TokKind::Star =>
                {
                    self.eat(&mut stream)?;
                    let t = self.expect_token()?;
                    self.assert_eof(
                        &stream,
                        "expected no more tokens after array size",
                    );
                    let span = t.span;
                    match t.kind {
                        TokKind::Question => ast::ArraySize::Within(
                            ast::Expr {
                                kind: ast::ExprKind::Int(0),
                                span,
                            },
                            ast::Expr {
                                kind: ast::ExprKind::Int(1),
                                span,
                            },
                        ),
                        TokKind::Plus => ast::ArraySize::AtLeast(ast::Expr {
                            kind: ast::ExprKind::Int(1),
                            span,
                        }),
                        TokKind::Star => ast::ArraySize::AtLeast(ast::Expr {
                            kind: ast::ExprKind::Int(0),
                            span,
                        }),
                        _ => unreachable!(),
                    }
                }
                TokTree::Delim(dn) if dn.delim == Brace => {
                    self.eat(&mut stream)?;
                    let dn = self.expect_delim()?;
                    let mut bounds_stream = dn.stream;

                    let lb_stream = bounds_stream.eat_until(&TokKind::Comma);
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
                kind: ast::ExprKind::Int(0),
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
        self.expect_kind(TokKind::Keyword(Keyword::In))?;

        self.eat(&mut stream)?; // first identifier
        let sa = ast::SymAccess::Sym(self.expect_ident()?);
        let mut arr = vec![sa];
        self.parse_sym_accesses(&mut stream, &mut arr)?;

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
                TokKind::Dot => {
                    let mut accs =
                        vec![ast::SymAccess::Sym(self.symtab.sym_self())];
                    self.parse_sym_accesses(stream, &mut accs)?;
                    ast::ExprKind::Ident(accs)
                }
                TokKind::Literal(LitKind::Int(val)) => ast::ExprKind::Int(val),
                TokKind::Ident(id) => {
                    let mut accs = vec![ast::SymAccess::Sym(id)];
                    self.parse_sym_accesses(stream, &mut accs)?;
                    ast::ExprKind::Ident(accs)
                }
                TokKind::Minus => {
                    let op = ast::UnOpKind::Neg;
                    let expr = self.parse_expr_fix(stream, op.fixity())?;
                    ast::ExprKind::Unary(Box::new(ast::UnOp {
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
                TokTree::Token(token) => {
                    let kind = match token.kind {
                        TokKind::Plus => ast::BinOpKind::Add,
                        TokKind::Minus => ast::BinOpKind::Sub,
                        TokKind::Star => ast::BinOpKind::Mul,
                        TokKind::Slash => ast::BinOpKind::Div,
                        TokKind::Percentage => ast::BinOpKind::Rem,
                        TokKind::Ampersand => ast::BinOpKind::BitAnd,
                        TokKind::Caret => ast::BinOpKind::BitXor,
                        TokKind::Pipe => ast::BinOpKind::BitOr,
                        TokKind::Eq2 => ast::BinOpKind::Eq,
                        TokKind::Neq => ast::BinOpKind::Neq,
                        TokKind::Ampersand2 => ast::BinOpKind::And,
                        TokKind::Pipe2 => ast::BinOpKind::Or,
                        TokKind::Lt => ast::BinOpKind::Lt,
                        TokKind::Gt => ast::BinOpKind::Gt,
                        TokKind::Leq => ast::BinOpKind::Leq,
                        TokKind::Geq => ast::BinOpKind::Geq,
                        TokKind::Lt2 => ast::BinOpKind::Shl,
                        TokKind::Gt2 => ast::BinOpKind::Shr,
                        _ => {
                            let kind = token.kind.clone();
                            self.eat(stream)?; // invalid token
                            return Err(self.err_hint(
                                UnexpectedToken(kind),
                                "expected binary operator",
                            ));
                        }
                    };

                    let (lfix, rfix) = kind.fixity();
                    if lfix < min_fixity {
                        break;
                    }

                    self.eat(stream)?; // bin op
                    let lhs = ast::Expr {
                        kind: lhs_kind,
                        span: lhs_span,
                    };
                    let rhs = self.parse_expr_fix(stream, rfix)?;
                    lhs_span.1 = rhs.span.1;
                    lhs_kind = ast::ExprKind::Binary(Box::new(ast::BinOp {
                        lhs,
                        rhs,
                        kind,
                    }));
                }
                _ => {
                    return Err(
                        self.err_hint(Unexpected, "expected binary operator")
                    );
                }
            }
        }

        Ok(ast::Expr {
            kind: lhs_kind,
            span: lhs_span,
        })
    }

    fn parse_sym_accesses(
        &mut self,
        stream: &mut TokenStream,
        accs: &mut Vec<ast::SymAccess>,
    ) -> PResult<()> {
        while stream.not_empty() {
            match stream.peek().unwrap() {
                TokTree::Token(token) if token.kind == TokKind::Dot => {
                    self.eat(stream)?; // dot
                    self.eat(stream)?;
                    accs.push(ast::SymAccess::Sym(self.expect_ident()?));
                }
                TokTree::Delim(dn) if dn.delim == Bracket => {
                    self.eat(stream)?; // bracket stream
                    let dn = self.expect_delim()?;
                    accs.push(ast::SymAccess::Index(
                        self.parse_expr(dn.stream)?,
                    ));
                }
                _ => break,
            }
        }

        Ok(())
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
        ("s8", []) => ast::FieldKind::Prim(ast::PrimType::S8),
        ("u16", []) => ast::FieldKind::Prim(ast::PrimType::U16),
        ("s16", []) => ast::FieldKind::Prim(ast::PrimType::S16),
        ("u32", []) => ast::FieldKind::Prim(ast::PrimType::U32),
        ("s32", []) => ast::FieldKind::Prim(ast::PrimType::S32),
        ("u64", []) => ast::FieldKind::Prim(ast::PrimType::U64),
        ("s64", []) => ast::FieldKind::Prim(ast::PrimType::S64),
        ("u128", []) => ast::FieldKind::Prim(ast::PrimType::U128),
        ("s128", []) => ast::FieldKind::Prim(ast::PrimType::S128),
        ("f32", []) => ast::FieldKind::Prim(ast::PrimType::F32),
        ("f64", []) => ast::FieldKind::Prim(ast::PrimType::F64),
        ("signed", [len]) => {
            ast::FieldKind::Prim(ast::PrimType::Signed(len.clone()))
        }
        ("unsigned", [len]) => {
            ast::FieldKind::Prim(ast::PrimType::Unsigned(len.clone()))
        }
        ("float", [ex, mt]) => {
            ast::FieldKind::Prim(ast::PrimType::Float(ex.clone(), mt.clone()))
        }
        ("bitvec", [len]) => {
            ast::FieldKind::Prim(ast::PrimType::BitVec(len.clone()))
        }
        _ => return None,
    };

    Some(kind)
}
