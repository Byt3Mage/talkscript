#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub(crate) enum TokenType {
    // Literals
    Ident = 0,
    IntLit,
    FloatLit,
    StringLit,

    // Single symbols
    RightParen,
    LeftParen,
    RightBrace,
    LeftBrace,
    RightBracket,
    LeftBracket,
    Comma,
    Dot,
    Semicolon,
    Question,

    // Single or double symbols
    Arrow,
    Bang,
    BangEqual,
    Colon,
    ColonColon,
    Equal,
    EqualEqual,
    FatArrow,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Minus,
    MinusEqual,
    Percent,
    PercentEqual,
    Plus,
    PlusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,

    // Keywords
    And,
    Any,
    Bool,
    Break,
    Continue,
    Else,
    False,
    Float,
    Fn,
    For,
    If,
    Int,
    Let,
    Match,
    Null,
    Or,
    Return,
    Struct,
    Str,
    This,
    True,
    While,
    Void,

    // End of file
    Eof,
}

#[macro_export]
macro_rules! token {
    (ident) => {
        $crate::token::TokenType::Ident
    };

    (int_lit) => {
        $crate::token::TokenType::IntLit
    };

    (float_lit) => {
        $crate::token::TokenType::FloatLit
    };

    (str_lit) => {
        $crate::token::TokenType::StringLit
    };

    (')') => {
        $crate::token::TokenType::RightParen
    };
    ('(') => {
        $crate::token::TokenType::LeftParen
    };
    ('}') => {
        $crate::token::TokenType::RightBrace
    };
    ('{') => {
        $crate::token::TokenType::LeftBrace
    };
    (']') => {
        $crate::token::TokenType::RightBracket
    };
    ('[') => {
        $crate::token::TokenType::LeftBracket
    };
    (,) => {
        $crate::token::TokenType::Comma
    };
    (.) => {
        $crate::token::TokenType::Dot
    };
    (;) => {
        $crate::token::TokenType::Semicolon
    };
    (?) => {
        $crate::token::TokenType::Question
    };

    // Single/double symbols
    (->) => {
        $crate::token::TokenType::Arrow
    };
    (!) => {
        $crate::token::TokenType::Bang
    };
    (!=) => {
        $crate::token::TokenType::BangEqual
    };
    (:) => {
        $crate::token::TokenType::Colon
    };
    (::) => {
        $crate::token::TokenType::ColonColon
    };
    (=) => {
        $crate::token::TokenType::Equal
    };
    (==) => {
        $crate::token::TokenType::EqualEqual
    };
    (>) => {
        $crate::token::TokenType::Greater
    };
    (>=) => {
        $crate::token::TokenType::GreaterEqual
    };
    (=>) => {
        $crate::token::TokenType::FatArrow
    };
    (<) => {
        $crate::token::TokenType::Less
    };
    (<=) => {
        $crate::token::TokenType::LessEqual
    };
    (-) => {
        $crate::token::TokenType::Minus
    };
    (-=) => {
        $crate::token::TokenType::MinusEqual
    };
    (%) => {
        $crate::token::TokenType::Percent
    };
    (%=) => {
        $crate::token::TokenType::PercentEqual
    };
    (+) => {
        $crate::token::TokenType::Plus
    };
    (+=) => {
        $crate::token::TokenType::PlusEqual
    };
    (*) => {
        $crate::token::TokenType::Star
    };
    (*=) => {
        $crate::token::TokenType::StarEqual
    };
    (/) => {
        $crate::token::TokenType::Slash
    };
    (/=) => {
        $crate::token::TokenType::SlashEqual
    };

    // Keywords
    (and) => {
        $crate::token::TokenType::And
    };
    (any) => {
        $crate::token::TokenType::Any
    };
    (bool) => {
        $crate::token::TokenType::Bool
    };
    (break) => {
        $crate::token::TokenType::Break
    };
    (continue) => {
        $crate::token::TokenType::Continue
    };
    (else) => {
        $crate::token::TokenType::Else
    };
    (false) => {
        $crate::token::TokenType::False
    };
    (float) => {
        $crate::token::TokenType::Float
    };
    (fn) => {
        $crate::token::TokenType::Fn
    };
    (for) => {
        $crate::token::TokenType::For
    };
    (if) => {
        $crate::token::TokenType::If
    };
    (int) => {
        $crate::token::TokenType::Int
    };
    (let) => {
        $crate::token::TokenType::Let
    };
    (match) => {
        $crate::token::TokenType::Match
    };
    (null) => {
        $crate::token::TokenType::Null
    };
    (or) => {
        $crate::token::TokenType::Or
    };
    (return) => {
        $crate::token::TokenType::Return
    };
    (str) => {
        $crate::token::TokenType::Str
    };
    (struct) => {
        $crate::token::TokenType::Struct
    };
    (self) => {
        $crate::token::TokenType::This
    };
    (true) => {
        $crate::token::TokenType::True
    };
    (while) => {
        $crate::token::TokenType::While
    };
    (void) => {
        $crate::token::TokenType::Void
    };
    (eof) => {
        $crate::token::TokenType::Eof
    };
}

impl TokenType {
    pub const COUNT: usize = TokenType::Eof as usize + 1;
}

/// Represents a region of source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Byte offset of first character (0-based)
    pub start: usize,
    /// Byte offset after last character (exclusive)
    pub end: usize,
    /// Line number (1-based)
    pub line: usize,
    /// Column number (1-based, in UTF-8 chars)
    pub column: usize,
}

impl Span {
    /// Creates a new span from individual components
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Merges two spans into one that covers both
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
            column: self.column,
        }
    }

    /// Returns a zero-length span at a position
    pub fn at_pos(byte_pos: usize, line: usize, column: usize) -> Self {
        Self {
            start: byte_pos,
            end: byte_pos,
            line,
            column,
        }
    }

    /// Checks if this span contains a byte position
    pub fn contains(&self, pos: usize) -> bool {
        self.start <= pos && pos < self.end
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Token<'a> {
    pub(crate) ty: TokenType,
    pub(crate) lexeme: &'a str,
    pub(crate) span: Span,
}
