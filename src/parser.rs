use crate::ast::{
    BinaryOp, CompoundOp, Expr, ExprKind, Field, Item, ItemKind, Param, Stmt, StmtKind, Type,
    UnaryOp,
};
use crate::lexer::{LexError, Lexer};
use crate::parse_rules::{InfixRule, ParseRule, Precedence, PrefixRule};
use crate::token;
use crate::token::{Span, Token, TokenType};
use crate::vm::vm_types::{Float, Integer};

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    span: Span,
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current: Token<'a>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer<'a>) -> Result<Parser<'a>, LexError> {
        let current = lexer.next_token()?;
        Ok(Parser { lexer, current })
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

    #[inline]
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

    fn parse_empty_stmt(&mut self) -> Result<Stmt, ParseError> {
        let semi = self.expect(token![;])?;
        Ok(Stmt {
            kind: StmtKind::Semi,
            span: semi.span,
        })
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParseError> {
        let let_token = self.expect(token![let])?;
        let name = self.expect(token![ident])?;
        let ty = if self.advance_if(token![:])? {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(token![=])?;
        let value = self.parse_expr()?;
        let semi_token = self.expect(token![;])?;

        Ok(Stmt {
            kind: StmtKind::Let {
                name: name.lexeme.into(),
                ty,
                value,
            },
            span: let_token.span.merge(semi_token.span),
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let type_token = self.consume()?;

        match type_token.ty {
            token![any] => Ok(Type::Any),
            token![!] => Ok(Type::Never), // TODO: maybe prevent manual specification.
            token![int] => Ok(Type::Int),
            token![float] => Ok(Type::Float),
            token![str] => Ok(Type::String),
            token![bool] => Ok(Type::Bool),
            token![void] => Ok(Type::Void),
            token![ident] => Ok(Type::Symbol(type_token.lexeme.into())),
            _ => Err(ParseError {
                msg: format!("Expected <type>, found {:?}", type_token.ty),
                span: type_token.span,
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.current.ty {
            token!['{'] => self.parse_block_expr(),
            token![if] => self.parse_if_expr(),
            token![while] => todo!("parse while expression"),
            token![match] => todo!("parse while expression"),
            _ => self.parse_precedence(Precedence::Assignment),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        let if_token = self.expect(token![if])?;
        let cond = self.parse_expr()?;
        let then_branch = self.parse_block_expr()?;
        let (else_branch, span) = if self.advance_if(token![else])? {
            if self.check(token![if]) {
                let expr = self.parse_if_expr()?;
                let span = if_token.span.merge(expr.span);
                (Some(Box::new(expr)), span)
            } else {
                let expr = self.parse_block_expr()?;
                let span = if_token.span.merge(expr.span);
                (Some(Box::new(expr)), span)
            }
        } else {
            (None, if_token.span.merge(then_branch.span))
        };

        Ok(Expr {
            kind: ExprKind::If {
                condition: Box::new(cond),
                then_branch: Box::new(then_branch),
                else_branch,
            },
            span,
        })
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParseError> {
        let l_brace = self.expect(token!['{'])?;

        let mut statements = vec![];
        let mut trailing_expr = None;

        'block: while self.current.ty != token!['}'] && self.current.ty != token![eof] {
            match self.current.ty {
                token![;] => statements.push(self.parse_empty_stmt()?),
                token![let] => statements.push(self.parse_let_stmt()?),
                _ => {
                    let mut is_block_expr = true;
                    let expr = match self.current.ty {
                        token!['{'] => self.parse_block_expr()?,
                        token![if] => self.parse_if_expr()?,
                        token![while] => todo!("parse while expression"),
                        token![match] => todo!("parse while expression"),
                        _ => {
                            is_block_expr = false;
                            self.parse_precedence(Precedence::Assignment)?
                        }
                    };

                    if is_block_expr {
                        let span = if self.check(token![;]) {
                            let semi = self.consume()?;
                            expr.span.merge(semi.span)
                        } else {
                            expr.span
                        };

                        statements.push(Stmt {
                            kind: StmtKind::Expr(expr),
                            span,
                        })
                    } else if self.check(token![;]) {
                        let semi = self.consume()?;
                        let span = expr.span.merge(semi.span);

                        statements.push(Stmt {
                            kind: StmtKind::Expr(expr),
                            span,
                        });
                    } else {
                        trailing_expr = Some(Box::new(expr));
                        break 'block;
                    }
                }
            }
        }

        let r_brace = self.expect(token!['}'])?;

        Ok(Expr {
            kind: ExprKind::Block {
                statements,
                trailing_expr,
            },
            span: l_brace.span.merge(r_brace.span),
        })
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        let rule = ParseRule::get(self.current.ty);
        let mut expr = self.parse_prefix(rule.prefix)?;

        loop {
            // 1. Check for `=` first (lowest precedence)
            if precedence <= Precedence::Assignment && self.check(token![=]) {
                expr = self.parse_assign(expr)?;
            }
            // 2. Check for compound assignments (`+=`, etc.)
            else if precedence <= Precedence::Assignment && self.is_compound_assign() {
                expr = self.parse_compound_assign(expr)?;
            }
            // 3. Handle regular binary operators (`+`, `*`, etc.)
            else {
                let infix_rule = ParseRule::get(self.current.ty);

                if precedence <= infix_rule.precedence && infix_rule.infix != InfixRule::None {
                    expr = self.parse_infix(infix_rule.infix, expr)?;
                } else {
                    break;
                }
            }
        }

        Ok(expr)
    }

    pub fn parse_prefix(&mut self, rule: PrefixRule) -> Result<Expr, ParseError> {
        match rule {
            PrefixRule::LiteralInt => self.parse_int(),
            PrefixRule::LiteralFloat => self.parse_float(),
            PrefixRule::LiteralString => self.parse_string(),
            PrefixRule::LiteralBool => self.parse_bool(),
            PrefixRule::LiteralNull => self.parse_null(),
            PrefixRule::LiteralVoid => self.parse_void(),
            PrefixRule::Identifier => self.parse_ident(),
            PrefixRule::Grouping => self.parse_group(),
            PrefixRule::Unary => self.parse_unary(),
            PrefixRule::None => Err(ParseError {
                msg: format!("Expected expression, found {:?}", self.current.ty),
                span: self.current.span,
            }),
        }
    }

    pub fn parse_infix(&mut self, rule: InfixRule, left: Expr) -> Result<Expr, ParseError> {
        match rule {
            InfixRule::Binary => self.parse_binary(left),
            InfixRule::Call => todo!(),
            InfixRule::Dot => todo!(),
            InfixRule::None => Err(ParseError {
                msg: format!("Expected expression, found {:?}", self.current.ty),
                span: self.current.span,
            }),
        }
    }

    fn parse_int(&mut self) -> Result<Expr, ParseError> {
        let int_token = self.expect(token![int_lit])?;

        match str::parse::<Integer>(int_token.lexeme) {
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

    fn parse_float(&mut self) -> Result<Expr, ParseError> {
        let float_token = self.expect(token![float_lit])?;

        match str::parse::<Float>(float_token.lexeme) {
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

    fn parse_string(&mut self) -> Result<Expr, ParseError> {
        let str_token = self.expect(token![str_lit])?;

        Ok(Expr {
            kind: ExprKind::String(str_token.lexeme.into()),
            span: str_token.span,
        })
    }

    fn parse_bool(&mut self) -> Result<Expr, ParseError> {
        let bool_token = self.consume()?;

        let value = match bool_token.ty {
            token![true] => true,
            token![false] => false,
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

    fn parse_null(&mut self) -> Result<Expr, ParseError> {
        let null_token = self.expect(token![null])?;
        Ok(Expr {
            kind: ExprKind::Null,
            span: null_token.span,
        })
    }

    fn parse_void(&mut self) -> Result<Expr, ParseError> {
        let void_token = self.expect(token![void])?;

        Ok(Expr {
            kind: ExprKind::Void,
            span: void_token.span,
        })
    }

    fn parse_ident(&mut self) -> Result<Expr, ParseError> {
        let id_token = self.expect(token![ident])?;

        Ok(Expr {
            kind: ExprKind::Ident(id_token.lexeme.into()),
            span: id_token.span,
        })
    }

    fn parse_group(&mut self) -> Result<Expr, ParseError> {
        let open = self.expect(token!['('])?;
        let expr = self.parse_expr()?;
        let close = self.expect(token![')'])?;

        Ok(Expr {
            kind: ExprKind::Group(Box::new(expr)),
            span: open.span.merge(close.span),
        })
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let op_token = self.consume()?;

        let op = match op_token.ty {
            token![-] => UnaryOp::Neg,
            token![!] => UnaryOp::Not,
            _ => {
                return Err(ParseError {
                    msg: format!("Unsupported unary operator: {:?}", op_token.ty),
                    span: op_token.span,
                });
            }
        };

        let expr = self.parse_expr()?;
        let span = op_token.span.merge(expr.span);

        Ok(Expr {
            kind: ExprKind::Unary {
                op,
                expr: Box::new(expr),
            },
            span,
        })
    }

    fn parse_binary(&mut self, lhs: Expr) -> Result<Expr, ParseError> {
        let op_token = self.consume()?;
        let op = match op_token.ty {
            token![+] => BinaryOp::Add,
            token![-] => BinaryOp::Sub,
            token![*] => BinaryOp::Mul,
            token![/] => BinaryOp::Div,
            token![%] => BinaryOp::Mod,
            token![==] => BinaryOp::Equal,
            token![!=] => BinaryOp::NotEqual,
            token![<] => BinaryOp::Less,
            token![<=] => BinaryOp::LessEqual,
            token![>] => BinaryOp::Greater,
            token![>=] => BinaryOp::GreaterEqual,
            _ => {
                return Err(ParseError {
                    msg: format!("Unsupported binary operator: {:?}", op_token.ty),
                    span: op_token.span,
                });
            }
        };

        let precedence = ParseRule::get(op_token.ty).precedence;
        let rhs = self.parse_precedence(precedence.next())?;
        let span = lhs.span.merge(rhs.span);

        Ok(Expr {
            kind: ExprKind::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            span,
        })
    }

    fn parse_assign(&mut self, target: Expr) -> Result<Expr, ParseError> {
        if !target.is_assignable() {
            return Err(ParseError {
                msg: "Invalid assignment target".into(),
                span: target.span,
            });
        }

        self.expect(token![=])?;
        let value = self.parse_precedence(Precedence::Assignment)?;
        let span = target.span.merge(value.span);

        Ok(Expr {
            kind: ExprKind::Assign {
                target: Box::new(target),
                value: Box::new(value),
            },
            span,
        })
    }

    fn is_compound_assign(&self) -> bool {
        matches!(
            self.current.ty,
            token![%=] | token![+=] | token![-=] | token![*=] | token![/=]
        )
    }

    fn parse_compound_assign(&mut self, target: Expr) -> Result<Expr, ParseError> {
        if !target.is_assignable() {
            return Err(ParseError {
                msg: "Invalid assignment target".into(),
                span: target.span,
            });
        }

        let op_token = self.consume()?;
        let op = match op_token.ty {
            token![+=] => CompoundOp::AddAssign,
            token![-=] => CompoundOp::SubAssign,
            token![*=] => CompoundOp::MulAssign,
            token![/=] => CompoundOp::DivAssign,
            token![%=] => CompoundOp::ModAssign,
            _ => {
                return Err(ParseError {
                    msg: format!("Invalid assignment operator: {:?}", op_token.ty),
                    span: op_token.span,
                });
            }
        };

        let value = self.parse_precedence(Precedence::Assignment)?;
        let span = target.span.merge(value.span);

        Ok(Expr {
            kind: ExprKind::CompoundAssign {
                op,
                target: Box::new(target),
                value: Box::new(value),
            },
            span,
        })
    }

    fn parse_function_item(&mut self) -> Result<Item, ParseError> {
        let fn_token = self.expect(token![fn])?;
        let fn_name = self.expect(token![ident])?;

        self.expect(token!['('])?;

        let mut params = vec![];
        while self.current.ty != token![')'] && self.current.ty != token![eof] {
            let param_name = self.expect(token![ident])?;
            self.expect(token![:])?;
            let param_ty = self.parse_type()?;

            params.push(Param {
                name: param_name.lexeme.into(),
                ty: param_ty,
            });

            if !self.advance_if(token![,])? {
                break;
            }
        }

        self.expect(token![')'])?;

        let ret = if self.advance_if(token![->])? {
            self.parse_type()?
        } else {
            Type::Void
        };

        let body = self.parse_block_expr()?;
        let span = fn_token.span.merge(body.span);

        Ok(Item {
            kind: ItemKind::Function {
                name: fn_name.lexeme.into(),
                params,
                ret,
                body,
            },
            span,
        })
    }

    fn parse_struct_item(&mut self) -> Result<Item, ParseError> {
        let struct_token = self.expect(token![struct])?;
        let struct_name = self.expect(token![ident])?;

        if self.check(token![;]) {
            let semi = self.consume()?;
            return Ok(Item {
                kind: ItemKind::Struct {
                    name: struct_name.lexeme.into(),
                    fields: vec![],
                },
                span: struct_token.span.merge(semi.span),
            });
        }

        self.expect(token!['{'])?;

        let mut fields = vec![];

        while self.current.ty != token!['}'] && self.current.ty != token![eof] {
            let name = self.expect(token![ident])?;
            self.expect(token![:])?;
            let ty = self.parse_type()?;

            fields.push(Field {
                name: name.lexeme.into(),
                ty,
            });

            if !self.advance_if(token![,])? {
                break;
            }
        }

        let r_brace = self.expect(token!['}'])?;

        Ok(Item {
            kind: ItemKind::Struct {
                name: struct_name.lexeme.into(),
                fields,
            },
            span: struct_token.span.merge(r_brace.span),
        })
    }
}

#[test]
fn test_parser() {
    let input = r#"
        fn add(a: int, b: int, c: float) -> int {
            a += 5;
            a + b
        }

        struct Point {
            x: float,
            y: float,
            debug: str,
        }
    "#;

    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer).unwrap();
    let function = parser.parse_function_item().unwrap();
    let strukt = parser.parse_struct_item().unwrap();

    println!("{:?}", strukt);
}
