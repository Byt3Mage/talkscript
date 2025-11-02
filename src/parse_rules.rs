use crate::token;
use crate::token::TokenType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixRule {
    None,
    LiteralInt,
    LiteralFloat,
    LiteralString,
    LiteralBool,
    LiteralNull,
    LiteralVoid,
    Identifier,
    Grouping,
    Unary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixRule {
    None,
    Binary,
    Call,
    Dot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    None = 0,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // ==, !=
    Comparison, // <, >, <=, >=
    Term,       // +, -
    Factor,     // *, /, %
    Unary,      // !, -
    Call,       // ., ()
    Primary,
}

impl Precedence {
    pub(crate) fn next(&self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParseRule {
    pub(crate) prefix: PrefixRule,
    pub(crate) infix: InfixRule,
    pub(crate) precedence: Precedence,
}

impl ParseRule {
    pub(crate) fn get(ty: TokenType) -> &'static Self {
        &RULES[ty as usize]
    }
}

static RULES: [ParseRule; TokenType::COUNT] = {
    let mut rules = [ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::None,
        precedence: Precedence::None,
    }; TokenType::COUNT];

    rules[TokenType::Ident as usize] = ParseRule {
        prefix: PrefixRule::Identifier,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules[TokenType::IntLit as usize] = ParseRule {
        prefix: PrefixRule::LiteralInt,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules[TokenType::FloatLit as usize] = ParseRule {
        prefix: PrefixRule::LiteralFloat,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules[TokenType::StringLit as usize] = ParseRule {
        prefix: PrefixRule::LiteralString,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules[TokenType::Null as usize] = ParseRule {
        prefix: PrefixRule::LiteralNull,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules[token![void] as usize] = ParseRule {
        prefix: PrefixRule::LiteralNull,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules[TokenType::LeftParen as usize] = ParseRule {
        prefix: PrefixRule::Grouping,
        infix: InfixRule::Call,
        precedence: Precedence::Call,
    };

    rules[TokenType::Dot as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Dot,
        precedence: Precedence::Call,
    };

    rules[TokenType::Percent as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Factor,
    };

    rules[TokenType::Plus as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Term,
    };

    rules[TokenType::Star as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Factor,
    };

    rules[TokenType::Slash as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Factor,
    };

    rules[TokenType::Bang as usize] = ParseRule {
        prefix: PrefixRule::Unary,
        infix: InfixRule::None,
        precedence: Precedence::None,
    };

    rules[TokenType::BangEqual as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Equality,
    };

    rules[TokenType::EqualEqual as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Equality,
    };

    rules[TokenType::Greater as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Comparison,
    };

    rules[TokenType::GreaterEqual as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Comparison,
    };

    rules[TokenType::Less as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Comparison,
    };

    rules[TokenType::LessEqual as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Comparison,
    };

    rules[TokenType::Minus as usize] = ParseRule {
        prefix: PrefixRule::Unary,
        infix: InfixRule::Binary,
        precedence: Precedence::Term,
    };

    rules[token![=] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::None,
        precedence: Precedence::Assignment,
    };
    rules[token![+=] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::None,
        precedence: Precedence::Assignment,
    };

    rules[token![-=] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::None,
        precedence: Precedence::Assignment,
    };

    rules[token![*=] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::None,
        precedence: Precedence::Assignment,
    };

    rules[token![/=] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::None,
        precedence: Precedence::Assignment,
    };

    rules[token![%=] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::None,
        precedence: Precedence::Assignment,
    };

    rules[token![and] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::And,
    };

    rules[token![false] as usize] = ParseRule {
        prefix: PrefixRule::LiteralBool,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules[token![or] as usize] = ParseRule {
        prefix: PrefixRule::None,
        infix: InfixRule::Binary,
        precedence: Precedence::Or,
    };

    rules[TokenType::True as usize] = ParseRule {
        prefix: PrefixRule::LiteralBool,
        infix: InfixRule::None,
        precedence: Precedence::Primary,
    };

    rules
};
