use super::tokens::{Span, Token, TokenType};
use crate::tt;

// Error Types
#[derive(Debug)]
pub enum LexErrorKind {
    UnexpectedChar(char),
    UnterminatedString,
    InvalidEscape(char),
    MalformedNumber,
}

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
    pub hint: Option<String>,
}

pub type LexResult<'a> = Result<Token<'a>, LexError>;

pub struct Lexer<'a> {
    input: &'a str,
    chars: std::str::CharIndices<'a>,
    current: Option<(usize, char)>,
    line: usize,
    column: usize,
    byte_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.char_indices();
        let current = chars.next();

        Self {
            input,
            chars,
            current,
            line: 1,
            column: 1,
            byte_pos: 0,
        }
    }

    fn advance(&mut self) {
        if let Some((pos, ch)) = self.current {
            self.byte_pos = pos + ch.len_utf8();

            if ch == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        self.current = self.chars.next();
    }

    #[inline]
    fn make_token(&self, ty: TokenType, start_byte: usize, lexeme: &'a str) -> Token<'a> {
        Token {
            ty,
            lexeme,
            span: Span {
                start: start_byte,
                end: self.byte_pos,
                line: self.line,
                column: self.column,
            },
        }
    }

    pub fn next_token(&mut self) -> LexResult<'a> {
        self.skip_whitespace();

        let Some((o, ch)) = self.current else {
            return Ok(self.make_token(tt![eof], self.byte_pos, "<eof>"));
        };

        match ch {
            // Single symbols
            '(' => Ok(self.make_single(tt!['('], o)),
            ')' => Ok(self.make_single(tt![')'], o)),
            '{' => Ok(self.make_single(tt!['{'], o)),
            '}' => Ok(self.make_single(tt!['}'], o)),
            '[' => Ok(self.make_single(tt!['['], o)),
            ']' => Ok(self.make_single(tt![']'], o)),
            ',' => Ok(self.make_single(tt![,], o)),
            '.' => Ok(self.make_single(tt![.], o)),
            ';' => Ok(self.make_single(tt![;], o)),
            '?' => Ok(self.make_single(tt![?], o)),

            // Double symbols
            '=' => Ok(self.make_equals(o)),
            '-' => Ok(self.make_minus(o)),
            ':' => Ok(self.make_double(':', tt![:], tt![::], o)),
            '!' => Ok(self.make_double('=', tt![!], tt![!=], o)),
            '>' => Ok(self.make_double('=', tt![>], tt![>=], o)),
            '<' => Ok(self.make_double('=', tt![<], tt![<=], o)),
            '%' => Ok(self.make_double('=', tt![%], tt![%=], o)),
            '+' => Ok(self.make_double('=', tt![+], tt![+=], o)),
            '*' => Ok(self.make_double('=', tt![*], tt![*=], o)),
            '/' => Ok(self.make_double('=', tt![/], tt![/=], o)),

            // String literals
            '"' => self.make_string(o),

            // Ints and float literals
            c if c.is_ascii_digit() => Ok(self.make_number(o)),

            // Idents
            c if c.is_ascii_alphabetic() || c == '_' => Ok(self.make_ident_or_kw(o)),

            // Syntax err.
            c => Err(LexError {
                kind: LexErrorKind::UnexpectedChar(c),
                span: Span::new(o, self.byte_pos, self.line, self.column),
                hint: match ch {
                    '@' | '$' => Some("This symbol is not used in the language".into()),
                    '`' => Some("Did you mean a single quote (')?".into()),
                    _ => None,
                },
            }),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some((_, ch)) = self.current {
            if ch.is_whitespace() {
                self.advance();
            } else if ch == '/' {
                if let Some((_, '/')) = self.peek_next() {
                    self.skip_line_comment();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn peek_next(&self) -> Option<(usize, char)> {
        self.chars.clone().next()
    }

    fn skip_line_comment(&mut self) {
        // Skip both '/' characters
        self.advance();
        self.advance();

        // Consume until newline or EOF
        while let Some((_, ch)) = self.current {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn make_single(&mut self, ty: TokenType, start_byte: usize) -> Token<'a> {
        self.advance();
        self.make_token(ty, start_byte, &self.input[start_byte..self.byte_pos])
    }

    fn make_double(
        &mut self,
        second_char: char,
        single_type: TokenType,
        double_type: TokenType,
        start_byte: usize,
    ) -> Token<'a> {
        self.advance();
        if let Some((_, ch)) = self.current
            && ch == second_char
        {
            self.advance();
            self.make_token(
                double_type,
                start_byte,
                &self.input[start_byte..self.byte_pos],
            )
        } else {
            self.make_token(
                single_type,
                start_byte,
                &self.input[start_byte..start_byte + 1],
            )
        }
    }

    fn make_equals(&mut self, start_byte: usize) -> Token<'a> {
        self.advance(); // Consume '='
        match self.current {
            Some((_, '=')) => {
                self.advance();
                self.make_token(tt![==], start_byte, &self.input[start_byte..self.byte_pos])
            }
            Some((_, '>')) => {
                self.advance();
                self.make_token(tt![=>], start_byte, &self.input[start_byte..self.byte_pos])
            }
            _ => self.make_token(tt![=], start_byte, &self.input[start_byte..start_byte + 1]),
        }
    }

    fn make_minus(&mut self, start_byte: usize) -> Token<'a> {
        self.advance(); // Consume '='

        match self.current {
            Some((_, '=')) => {
                self.advance();
                self.make_token(tt![-=], start_byte, &self.input[start_byte..self.byte_pos])
            }
            Some((_, '>')) => {
                self.advance();
                self.make_token(tt![->], start_byte, &self.input[start_byte..self.byte_pos])
            }
            _ => self.make_token(tt![-], start_byte, &self.input[start_byte..start_byte + 1]),
        }
    }

    fn make_string(&mut self, start_byte: usize) -> LexResult<'a> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip opening quote

        let mut escape = false;
        let mut end_byte;

        while let Some((byte, ch)) = self.current {
            end_byte = byte + ch.len_utf8();

            match ch {
                '\\' if !escape => {
                    escape = true;
                    self.advance();
                }
                '"' if !escape => {
                    self.advance();

                    return Ok(self.make_token(
                        TokenType::StringLit,
                        start_byte,
                        &self.input[start_byte..end_byte],
                    ));
                }
                '\n' if escape => break,
                _ => {
                    if escape {
                        match ch {
                            'n' | 't' | 'r' | '\\' | '"' | '\'' => {
                                escape = false;
                                self.advance();
                            }
                            _ => {
                                return Err(LexError {
                                    kind: LexErrorKind::InvalidEscape(ch),
                                    span: Span::new(byte, end_byte, self.line, self.column),
                                    hint: Some(r#"Valid escapes: \n, \t, \r, \\, \", \'"#.into()),
                                });
                            }
                        }
                    } else {
                        self.advance();
                    }
                }
            }
        }

        // EOF reached without closing quote
        Err(LexError {
            kind: LexErrorKind::UnterminatedString,
            span: Span::new(start_byte, self.byte_pos, start_line, start_col),
            hint: Some("Unterminated string. Did you forget a closing `\"`?".to_string()),
        })
    }

    fn make_number(&mut self, start_byte: usize) -> Token<'a> {
        let mut is_float = false;
        let mut end_byte = start_byte;

        while let Some((byte_pos, ch)) = self.current {
            match ch {
                '0'..='9' => {
                    end_byte = byte_pos + ch.len_utf8();
                    self.advance();
                }
                '.' if !is_float => {
                    is_float = true;
                    end_byte = byte_pos + ch.len_utf8();
                    self.advance();
                }
                _ => break,
            }
        }

        let ty = if is_float {
            TokenType::FloatLit
        } else {
            TokenType::IntLit
        };

        self.make_token(ty, start_byte, &self.input[start_byte..end_byte])
    }

    fn make_ident_or_kw(&mut self, start_byte: usize) -> Token<'a> {
        let mut end_byte = start_byte;

        while let Some((byte_pos, ch)) = self.current {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                end_byte = byte_pos + ch.len_utf8();
                self.advance();
            } else {
                break;
            }
        }

        let lexeme = &self.input[start_byte..end_byte];

        let ty = match lexeme {
            "and" => tt![and],
            "any" => tt![any],
            "bool" => tt![bool],
            "break" => tt![break],
            "continue" => tt![continue],
            "else" => tt![else],
            "false" => tt![false],
            "float" => tt![float],
            "fn" => tt![fn],
            "for" => tt![for],
            "if" => tt![if],
            "int" => tt![int],
            "loop" => tt![loop],
            "match" => tt![match],
            "mod" => tt![mod],
            "null" => tt![null],
            "or" => tt![or],
            "pub" => tt![pub],
            "return" => tt![return],
            "str" => tt![str],
            "struct" => tt![struct],
            "self" => tt![self],
            "true" => tt![true],
            "val" => tt![val],
            "var" => tt![var],
            "while" => tt![while],
            "void" => tt![void],
            _ => TokenType::Ident,
        };

        self.make_token(ty, start_byte, lexeme)
    }
}
