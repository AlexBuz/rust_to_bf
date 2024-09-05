use {crate::ast::Ident, chumsky::prelude::*};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // delimiters
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Comma,
    Dot,
    Semi,
    Colon,
    // arrows
    ThinArrow,
    FatArrow,
    // relational operators
    LtEq,
    Lt,
    GtEq,
    Gt,
    EqEq,
    BangEq,
    // assignment operators
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    AndAndEq,
    OrOrEq,
    Eq,
    // logical operators
    AndAnd,
    OrOr,
    Bang,
    // arithmetic operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    // references
    And,
    // keywords
    As,
    Let,
    Mut,
    Fn,
    Struct,
    If,
    Else,
    While,
    Loop,
    Match,
    Return,
    Break,
    Continue,
    Underscore,
    // literals
    True,
    False,
    Int(String),
    Char(char),
    String(String),
    // identifiers
    Ident(Ident),
}

fn delimiter_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice([
        just('{').to(Token::OpenBrace),
        just('}').to(Token::CloseBrace),
        just('[').to(Token::OpenBracket),
        just(']').to(Token::CloseBracket),
        just('(').to(Token::OpenParen),
        just(')').to(Token::CloseParen),
        just(',').to(Token::Comma),
        just('.').to(Token::Dot),
        just(';').to(Token::Semi),
        just(':').to(Token::Colon),
    ])
}

fn operator_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice([
        // arrows
        just("->").to(Token::ThinArrow),
        just("=>").to(Token::FatArrow),
        // relational operators
        just("<=").to(Token::LtEq),
        just("<").to(Token::Lt),
        just(">=").to(Token::GtEq),
        just(">").to(Token::Gt),
        just("==").to(Token::EqEq),
        just("!=").to(Token::BangEq),
        // assignment operators
        just("+=").to(Token::PlusEq),
        just("-=").to(Token::MinusEq),
        just("*=").to(Token::StarEq),
        just("/=").to(Token::SlashEq),
        just("%=").to(Token::PercentEq),
        just("&&=").to(Token::AndAndEq),
        just("||=").to(Token::OrOrEq),
        just("=").to(Token::Eq),
        // logical operators
        just("&&").to(Token::AndAnd),
        just("||").to(Token::OrOr),
        just("!").to(Token::Bang),
        // arithmetic operators
        just("+").to(Token::Plus),
        just("-").to(Token::Minus),
        just("*").to(Token::Star),
        just("/").to(Token::Slash),
        just("%").to(Token::Percent),
        // references
        just("&").to(Token::And),
    ])
}

fn keyword_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice([
        text::keyword("as").to(Token::As),
        text::keyword("let").to(Token::Let),
        text::keyword("mut").to(Token::Mut),
        text::keyword("fn").to(Token::Fn),
        text::keyword("struct").to(Token::Struct),
        text::keyword("if").to(Token::If),
        text::keyword("else").to(Token::Else),
        text::keyword("while").to(Token::While),
        text::keyword("loop").to(Token::Loop),
        text::keyword("match").to(Token::Match),
        text::keyword("return").to(Token::Return),
        text::keyword("break").to(Token::Break),
        text::keyword("continue").to(Token::Continue),
        text::keyword("_").to(Token::Underscore),
        text::keyword("true").to(Token::True),
        text::keyword("false").to(Token::False),
    ])
}

fn char_escape_lexer() -> impl Parser<char, char, Error = Simple<char>> + Clone {
    just('\\').ignore_then(choice([
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('\\').to('\\'),
        just('"').to('"'),
        just('\'').to('\''),
        just('0').to('\0'),
    ]))
}

fn string_literal_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    just('"')
        .ignore_then(none_of("\"\\").or(char_escape_lexer()).repeated().collect())
        .then_ignore(just('"'))
        .map(Token::String)
}

fn char_literal_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    just('\'')
        .ignore_then(none_of("'\\").or(char_escape_lexer()))
        .then_ignore(just('\''))
        .map(Token::Char)
}

fn int_literal_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    text::int(10).from_str().unwrapped().map(Token::Int)
}

fn ident_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    text::ident().map(Token::Ident)
}

fn token_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice((
        delimiter_lexer(),
        operator_lexer(),
        keyword_lexer(),
        string_literal_lexer(),
        char_literal_lexer(),
        int_literal_lexer(),
        ident_lexer(),
    ))
}

fn comment_lexer() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    just("//").then(none_of('\n').repeated()).padded().ignored()
}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    token_lexer()
        .padded_by(comment_lexer().repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}
