use simple_ternary::tnr;

use super::{
    ast::{
        AssignOp, AstArena, BinaryOp, Expr, ExprKind, Item, ItemKind, Param,
        Stmt, StmtKind, UnaryOp,
    },
    lexer::{LexError, Lexer},
    parse_rules::{InfixRule, ParseRule, Precedence, PrefixRule},
    tokens::{Span, Token, TokenType},
};

use crate::{
    arena::Interner,
    compiler::ast::{ItemId, Pattern, Visibility},
    tt,
};

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    span: Span,
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    interner: &'a mut Interner,
    ast: &'a mut AstArena,
    current: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(
        lexer: &'a mut Lexer<'a>,
        interner: &'a mut Interner,
        ast: &'a mut AstArena,
    ) -> Result<Parser<'a>, LexError> {
        let current = lexer.next_token()?;

        Ok(Parser {
            interner,
            lexer,
            current,
            ast,
        })
    }

    pub fn advance(&mut self) -> Result<(), ParseError> {
        match self.lexer.next_token() {
            Ok(token) => {
                self.current = token;
                Ok(())
            }
            Err(err) => Err(ParseError {
                msg: format!("Lexer error: {:?}", err),
                span: err.span,
            }),
        }
    }

    fn advance_if(&mut self, ty: TokenType) -> Result<bool, ParseError> {
        if self.current.ty == ty {
            self.advance()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume(&mut self) -> Result<Token<'a>, ParseError> {
        let token = self.current.clone();
        self.advance()?;
        Ok(token)
    }

    #[inline(always)]
    fn check(&self, ty: TokenType) -> bool {
        self.current.ty == ty
    }

    fn expect(&mut self, ty: TokenType) -> Result<Token<'a>, ParseError> {
        if self.current.ty == ty {
            self.consume()
        } else {
            Err(ParseError {
                msg: format!("Expected {:?}, found {:?}", ty, self.current.ty),
                span: self.current.span,
            })
        }
    }

    pub fn parse_source(&mut self) -> ParseResult<ItemId> {
        let mut items = vec![];

        while !self.check(tt![eof]) {
            let item = self.parse_item()?;
            items.push(self.ast.items.insert(item));
        }

        let item = Item {
            vis: Visibility::Public,
            name: self.interner.get_or_intern_static("<package>"),
            kind: ItemKind::Module { decls: items },
            span: Span::default().merge(self.current.span),
        };

        Ok(self.ast.items.insert(item))
    }

    fn parse_item(&mut self) -> ParseResult<Item> {
        let is_pub = self.advance_if(tt![pub])?;
        let vis = tnr! { is_pub => Visibility::Public : Visibility::Private};

        match self.current.ty {
            tt![mod] => self.parse_module(vis),
            tt![fn] => self.parse_function(vis),
            tt![struct] => self.parse_struct(vis),
            tt![type] => todo!("parse_type_alias(vis)"),
            tt => Err(ParseError {
                msg: format!("Expected item, found {tt:?}"),
                span: self.current.span,
            }),
        }
    }

    fn parse_module(&mut self, vis: Visibility) -> ParseResult<Item> {
        let mod_token = self.expect(tt![mod])?;
        let name = self.expect(tt![ident])?;
        self.expect(tt!['{'])?;

        let mut items = vec![];

        while !matches!(self.current.ty, tt!['}'] | tt![eof]) {
            let item = self.parse_item()?;
            items.push(self.ast.items.insert(item));
        }

        let r_brace = self.expect(tt!['}'])?;

        Ok(Item {
            vis,
            name: self.interner.get_or_intern(name.lexeme),
            kind: ItemKind::Module { decls: items },
            span: mod_token.span.merge(r_brace.span),
        })
    }

    fn parse_function(&mut self, is_pub: bool) -> ParseResult<Item> {
        let fn_token = self.expect(tt![fn])?;
        let func_name = self.expect(tt![ident])?;

        le
        self.expect(tt!['('])?;

        let mut generics

        let mut params = vec![];
        while !matches!(self.current.ty, tt![')'] | tt![eof]) {
            let pattern = self.parse_pattern()?;
            self.expect(tt![:])?;
            let ty = self.parse_type()?;

            params.push(Param {
                span: pattern.span.merge(ty.span),
                pattern: self.ast.patterns.insert(pattern),
                ty: self.ast.types.insert(ty),
            });

            if !self.advance_if(tt![,])? {
                break;
            }
        }

        let r_paren = self.expect(tt![')'])?;

        let ret = if self.advance_if(tt![->])? {
            self.parse_type()?
        } else {
            Type {
                kind: TypeKind::Void,
                span: r_paren.span,
            }
        };

        let body = self.parse_block()?;

        Ok(Item {
            vis,
            name: self.interner.get_or_intern(func_name.lexeme),
            span: fn_token.span.merge(body.span),
            kind: ItemKind::Function {
                params,
                ret: self.ast.types.insert(ret),
                body: self.ast.exprs.insert(body),
            },
        })
    }

    fn parse_struct(&mut self, is_pub: bool) -> ParseResult<Item> {
        let struct_token = self.expect(tt![struct])?;
        let struct_name = self.expect(tt![ident])?;

        if self.check(tt![;]) {
            let semi = self.consume()?;

            return Ok(Item {
                is_pub,
                name: self.interner.get_or_intern(struct_name.lexeme),
                kind: ItemKind::DataType { fields: vec![] },
                span: struct_token.span.merge(semi.span),
            });
        }

        self.expect(tt!['{'])?;

        let mut fields = vec![];
        while !matches!(self.current.ty, tt!['}'] | tt![eof]) {
            let name = self.expect(tt![ident])?;
            self.expect(tt![:])?;
            let ty = self.parse_type()?;

            fields.push(NamedField {
                span: name.span.merge(ty.span),
                name: self.interner.get_or_intern(name.lexeme),
                ty: self.ast.types.insert(ty),
            });

            if !self.advance_if(tt![,])? {
                break;
            }
        }

        let r_brace = self.expect(tt!['}'])?;

        Ok(Item {
            is_pub,
            name: self.interner.get_or_intern(struct_name.lexeme),
            kind: ItemKind::DataType { fields },
            span: struct_token.span.merge(r_brace.span),
        })
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        todo!("parse pattern")
    }

    fn parse_semi(&mut self) -> ParseResult<Stmt> {
        let semi_token = self.expect(tt![;])?;

        Ok(Stmt {
            kind: StmtKind::Semi,
            span: semi_token.span,
        })
    }

    fn parse_var_decl(&mut self, is_mut: bool) -> ParseResult<Stmt> {
        let val_token = if is_mut {
            self.expect(tt![var])?
        } else {
            self.expect(tt![val])?
        };

        let val_name = self.expect(tt![ident])?;

        let ty = if self.advance_if(tt![:])? {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(tt![=])?;

        let val = self.parse_expr()?;
        let semi_token = self.expect(tt![;])?;

        Ok(Stmt {
            kind: StmtKind::VarDecl {
                mutable: is_mut,
                name: self.interner.get_or_intern(val_name.lexeme),
                ty: ty.map(|t| self.ast.types.insert(t)),
                val: self.ast.exprs.insert(val),
            },
            span: val_token.span.merge(semi_token.span),
        })
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let type_token = self.consume()?;
        let mut span = type_token.span;

        let kind = match type_token.ty {
            tt![any] => TypeKind::Any,
            tt![!] => TypeKind::Never,
            tt![int] => TypeKind::Int,
            tt![float] => TypeKind::Float,
            tt![str] => TypeKind::String,
            tt![bool] => TypeKind::Bool,
            tt![void] => TypeKind::Void,
            tt![ident] => {
                let path = self.expect(tt![ident])?;
                TypeKind::Path(self.interner.get_or_intern(path.lexeme))
            }
            tt!['['] => {
                let inner = self.parse_type()?;
                let inner = self.ast.types.insert(inner);

                let kind = if self.advance_if(tt![;])? {
                    let len = self.parse_expr()?;

                    TypeKind::Array {
                        ty: inner,
                        len: self.ast.exprs.insert(len),
                    }
                } else {
                    TypeKind::DynArray(inner)
                };

                span = span.merge(self.expect(tt![']'])?.span);
                kind
            }
            _ => {
                return Err(ParseError {
                    msg: format!("Expected type, found {:?}", type_token.ty),
                    span: type_token.span,
                });
            }
        };

        let ty = Type { kind, span };

        if self.check(tt![?]) {
            let opt_token = self.consume()?;
            span = span.merge(opt_token.span);

            Ok(Type {
                kind: TypeKind::Optional(self.ast.types.insert(ty)),
                span,
            })
        } else {
            Ok(ty)
        }
    }

    fn parse_path(&mut self) -> ParseResult<Path> {
        todo!("parse path")
    }

    #[inline]
    fn can_start_expr(&self) -> bool {
        matches!(
            self.current.ty,
            tt![int_lit]
                | tt![float_lit]
                | tt![str_lit]
                | tt![true]
                | tt![false]
                | tt![null]
                | tt![void]
                | tt![ident]
                | tt!['(']
                | tt!['[']
                | tt!['{']
                | tt![if]
                | tt![while]
                | tt![loop]
                | tt![match]
                | tt![return]
                | tt![break]
                | tt![continue]
                | tt![!]
                | tt![-]
        )
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> ParseResult<Expr> {
        let rule = ParseRule::get(self.current.ty);
        let mut expr = self.parse_prefix(rule.prefix)?;

        loop {
            let infix_rule = ParseRule::get(self.current.ty);

            if precedence <= infix_rule.precedence && infix_rule.infix != InfixRule::None {
                expr = self.parse_infix(infix_rule.infix, expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_prefix(&mut self, rule: PrefixRule) -> ParseResult<Expr> {
        match rule {
            PrefixRule::None => Err(ParseError {
                msg: format!("Expected expression, found {:?}", self.current.ty),
                span: self.current.span,
            }),
            PrefixRule::LiteralInt => self.parse_int(),
            PrefixRule::LiteralFloat => self.parse_float(),
            PrefixRule::LiteralString => self.parse_string(),
            PrefixRule::LiteralBool => self.parse_bool(),
            PrefixRule::LiteralNull => self.parse_null(),
            PrefixRule::LiteralVoid => self.parse_void(),
            PrefixRule::LiteralArray => self.parse_array(),
            PrefixRule::Identifier => self.parse_ident(),
            PrefixRule::Grouping => self.parse_group(),
            PrefixRule::Unary => self.parse_unary(),
            PrefixRule::If => self.parse_if(),
            PrefixRule::Block => self.parse_block(),
            PrefixRule::While => self.parse_while(),
            PrefixRule::Loop => self.parse_loop(),
            PrefixRule::Match => todo!("parse match"),
            PrefixRule::Return => self.parse_return(),
            PrefixRule::Break => self.parse_break(),
            PrefixRule::Continue => self.parse_continue(),
        }
    }

    fn parse_infix(&mut self, rule: InfixRule, left: Expr) -> ParseResult<Expr> {
        match rule {
            InfixRule::Binary => self.parse_binary(left),
            InfixRule::Assign => self.parse_assign(left),
            InfixRule::Call => self.parse_call(left),
            InfixRule::Dot => self.parse_dot(left),
            InfixRule::Index => self.parse_index(left),
            InfixRule::None => Err(ParseError {
                msg: format!("Expected expression, found {:?}", self.current.ty),
                span: self.current.span,
            }),
        }
    }

    fn parse_int(&mut self) -> ParseResult<Expr> {
        let int_token = self.expect(tt![int_lit])?;

        match str::parse::<i64>(int_token.lexeme) {
            Ok(i) => Ok(Expr {
                kind: ExprKind::Int(i),
                span: int_token.span,
            }),
            Err(err) => Err(ParseError {
                msg: format!("Error parsing int literal: {err}"),
                span: int_token.span,
            }),
        }
    }

    fn parse_float(&mut self) -> ParseResult<Expr> {
        let float_token = self.expect(tt![float_lit])?;

        match str::parse::<f64>(float_token.lexeme) {
            Ok(f) => Ok(Expr {
                kind: ExprKind::Float(f),
                span: float_token.span,
            }),
            Err(err) => Err(ParseError {
                msg: format!("Error parsing float literal: {err}"),
                span: float_token.span,
            }),
        }
    }

    fn parse_string(&mut self) -> ParseResult<Expr> {
        let str_token = self.expect(tt![str_lit])?;

        Ok(Expr {
            kind: ExprKind::CStr(self.interner.get_or_intern(str_token.lexeme)),
            span: str_token.span,
        })
    }

    fn parse_bool(&mut self) -> ParseResult<Expr> {
        let bool_token = self.consume()?;

        let value = match bool_token.ty {
            tt![true] => true,
            tt![false] => false,
            _ => {
                return Err(ParseError {
                    msg: format!("Expected 'true' or 'false', found {:?}", self.current.ty),
                    span: bool_token.span,
                });
            }
        };

        Ok(Expr {
            kind: ExprKind::Bool(value),
            span: bool_token.span,
        })
    }

    fn parse_null(&mut self) -> ParseResult<Expr> {
        let null_token = self.expect(tt![null])?;

        Ok(Expr {
            kind: ExprKind::Null,
            span: null_token.span,
        })
    }

    fn parse_void(&mut self) -> ParseResult<Expr> {
        let void_token = self.expect(tt![void])?;

        Ok(Expr {
            kind: ExprKind::Void,
            span: void_token.span,
        })
    }

    fn parse_array(&mut self) -> ParseResult<Expr> {
        let l_brkt = self.expect(tt!['['])?;

        let mut elems = vec![];

        while !matches!(self.current.ty, tt![']'] | tt![eof]) {
            let elem = self.parse_expr()?;
            elems.push(self.ast.exprs.insert(elem));

            if !self.advance_if(tt![,])? {
                break;
            }
        }

        let r_brkt = self.expect(tt![']'])?;

        Ok(Expr {
            kind: ExprKind::ArrayLit(elems),
            span: l_brkt.span.merge(r_brkt.span),
        })
    }

    fn parse_ident(&mut self) -> ParseResult<Expr> {
        let ident_token = self.expect(tt![ident])?;

        Ok(Expr {
            kind: ExprKind::Ident(self.interner.get_or_intern(ident_token.lexeme)),
            span: ident_token.span,
        })
    }

    fn parse_group(&mut self) -> ParseResult<Expr> {
        let open = self.expect(tt!['('])?;
        let expr = self.parse_expr()?;
        let close = self.expect(tt![')'])?;

        Ok(Expr {
            kind: ExprKind::Group(self.ast.exprs.insert(expr)),
            span: open.span.merge(close.span),
        })
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        let op_token = self.consume()?;

        let op = match op_token.ty {
            tt![-] => UnaryOp::Neg,
            tt![!] => UnaryOp::Not,
            _ => {
                return Err(ParseError {
                    msg: format!("Unsupported unary operator: {:?}", op_token.ty),
                    span: op_token.span,
                });
            }
        };

        let expr = self.parse_precedence(Precedence::Unary)?;

        Ok(Expr {
            span: op_token.span.merge(expr.span),
            kind: ExprKind::Unary {
                op,
                expr: self.ast.exprs.insert(expr),
            },
        })
    }

    fn parse_binary(&mut self, lhs: Expr) -> ParseResult<Expr> {
        let op_token = self.consume()?;
        let op = match op_token.ty {
            tt![+] => BinaryOp::Add,
            tt![-] => BinaryOp::Sub,
            tt![*] => BinaryOp::Mul,
            tt![/] => BinaryOp::Div,
            tt![%] => BinaryOp::Mod,
            tt![==] => BinaryOp::Equal,
            tt![!=] => BinaryOp::NotEqual,
            tt![<] => BinaryOp::Less,
            tt![<=] => BinaryOp::LessEqual,
            tt![>] => BinaryOp::Greater,
            tt![>=] => BinaryOp::GreaterEqual,
            _ => {
                return Err(ParseError {
                    msg: format!("Unsupported binary operator: {:?}", op_token.ty),
                    span: op_token.span,
                });
            }
        };

        let precedence = ParseRule::get(op_token.ty).precedence;
        let rhs = self.parse_precedence(precedence.next())?;

        Ok(Expr {
            span: lhs.span.merge(rhs.span),
            kind: ExprKind::Binary {
                op,
                lhs: self.ast.exprs.insert(lhs),
                rhs: self.ast.exprs.insert(rhs),
            },
        })
    }

    fn parse_assign(&mut self, tgt: Expr) -> ParseResult<Expr> {
        let op_token = self.consume()?;
        let op = match op_token.ty {
            tt![=] => AssignOp::Assign,
            tt![+=] => AssignOp::AddAssign,
            tt![-=] => AssignOp::SubAssign,
            tt![*=] => AssignOp::MulAssign,
            tt![/=] => AssignOp::DivAssign,
            tt![%=] => AssignOp::ModAssign,
            _ => {
                return Err(ParseError {
                    msg: format!("Invalid assignment operator: {:?}", op_token.ty),
                    span: op_token.span,
                });
            }
        };

        let val = self.parse_precedence(Precedence::Assignment)?;

        Ok(Expr {
            span: tgt.span.merge(val.span),
            kind: ExprKind::Assign {
                op,
                tgt: self.ast.exprs.insert(tgt),
                val: self.ast.exprs.insert(val),
            },
        })
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
        let l_brace = self.expect(tt!['{'])?;
        let mut stmts = vec![];

        while !matches!(self.current.ty, tt!['}'] | tt![eof]) {
            match self.current.ty {
                tt![;] => {
                    let stmt = self.parse_semi()?;
                    stmts.push(self.ast.stmts.insert(stmt))
                }
                tt![val] => {
                    let stmt = self.parse_var_decl(false)?;
                    stmts.push(self.ast.stmts.insert(stmt))
                }
                tt![var] => {
                    let stmt = self.parse_var_decl(true)?;
                    stmts.push(self.ast.stmts.insert(stmt))
                }
                _ => {
                    let expr = self.parse_expr()?;
                    let mut span = expr.span;

                    let has_semi = if self.check(tt![;]) {
                        let semi_token = self.consume()?;
                        span = span.merge(semi_token.span);
                        true
                    } else {
                        false
                    };

                    let stmt = Stmt {
                        kind: StmtKind::Expr {
                            expr: self.ast.exprs.insert(expr),
                            has_semi,
                        },
                        span,
                    };

                    stmts.push(self.ast.stmts.insert(stmt));
                }
            }
        }

        let r_brace = self.expect(tt!['}'])?;

        Ok(Expr {
            kind: ExprKind::Block(stmts),
            span: l_brace.span.merge(r_brace.span),
        })
    }

    fn parse_if(&mut self) -> ParseResult<Expr> {
        let if_token = self.expect(tt![if])?;

        let cond = self.parse_expr()?;
        let then_branch = self.parse_block()?;

        let (else_branch, span) = if self.advance_if(tt![else])? {
            if self.check(tt![if]) {
                let expr = self.parse_if()?;
                let span = if_token.span.merge(expr.span);
                (Some(self.ast.exprs.insert(expr)), span)
            } else {
                let expr = self.parse_block()?;
                let span = if_token.span.merge(expr.span);
                (Some(self.ast.exprs.insert(expr)), span)
            }
        } else {
            (None, if_token.span.merge(then_branch.span))
        };

        Ok(Expr {
            kind: ExprKind::If {
                cond: self.ast.exprs.insert(cond),
                then_branch: self.ast.exprs.insert(then_branch),
                else_branch: else_branch,
            },
            span,
        })
    }

    fn parse_while(&mut self) -> ParseResult<Expr> {
        let while_token = self.expect(tt![while])?;
        let cond = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(Expr {
            span: while_token.span.merge(body.span),
            kind: ExprKind::While {
                cond: self.ast.exprs.insert(cond),
                body: self.ast.exprs.insert(body),
            },
        })
    }

    fn parse_loop(&mut self) -> ParseResult<Expr> {
        let loop_token = self.expect(tt![loop])?;
        let body = self.parse_block()?;

        Ok(Expr {
            span: loop_token.span.merge(body.span),
            kind: ExprKind::Loop(self.ast.exprs.insert(body)),
        })
    }

    fn parse_return(&mut self) -> ParseResult<Expr> {
        let return_token = self.expect(tt![return])?;

        let expr = if self.can_start_expr() {
            self.parse_expr()?
        } else {
            Expr {
                kind: ExprKind::Void,
                span: return_token.span,
            }
        };

        Ok(Expr {
            span: return_token.span.merge(expr.span),
            kind: ExprKind::Return(self.ast.exprs.insert(expr)),
        })
    }

    fn parse_break(&mut self) -> ParseResult<Expr> {
        let break_token = self.expect(tt![break])?;

        let expr = if self.can_start_expr() {
            self.parse_expr()?
        } else {
            Expr {
                kind: ExprKind::Void,
                span: break_token.span,
            }
        };

        Ok(Expr {
            span: break_token.span.merge(expr.span),
            kind: ExprKind::Break(self.ast.exprs.insert(expr)),
        })
    }

    fn parse_continue(&mut self) -> ParseResult<Expr> {
        let continue_token = self.expect(tt![continue])?;

        Ok(Expr {
            kind: ExprKind::Continue,
            span: continue_token.span,
        })
    }

    fn parse_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        self.expect(tt!['('])?;

        let mut args = vec![];
        while !matches!(self.current.ty, tt![')'] | tt![eof]) {
            let arg = self.parse_expr()?;
            args.push(self.ast.exprs.insert(arg));

            if !self.advance_if(tt![,])? {
                break;
            }
        }

        let r_paren = self.expect(tt![')'])?;

        Ok(Expr {
            span: callee.span.merge(r_paren.span),
            kind: ExprKind::Call {
                callee: self.ast.exprs.insert(callee),
                args,
            },
        })
    }

    fn parse_dot(&mut self, object: Expr) -> ParseResult<Expr> {
        self.expect(tt![.])?;
        let field = self.expect(tt![ident])?;

        Ok(Expr {
            span: object.span.merge(field.span),
            kind: ExprKind::Field {
                object: self.ast.exprs.insert(object),
                field: self.interner.get_or_intern(field.lexeme),
            },
        })
    }

    fn parse_index(&mut self, object: Expr) -> ParseResult<Expr> {
        self.expect(tt!['['])?;
        let index = self.parse_expr()?;
        let r_brkt = self.expect(tt![']'])?;

        Ok(Expr {
            span: object.span.merge(r_brkt.span),
            kind: ExprKind::Index {
                object: self.ast.exprs.insert(object),
                index: self.ast.exprs.insert(index),
            },
        })
    }
}
