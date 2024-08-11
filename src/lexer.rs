use {crate::ast::Ident, chumsky::prelude::*};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // delimiters
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Comma,
    Semi,
    // arrow operator
    Arrow,
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
    // keywords
    Let,
    Mut,
    Fn,
    If,
    Else,
    While,
    Loop,
    Match,
    Return,
    Break,
    Continue,
    // literals
    Int(usize),
    String(String),
    // identifiers
    Ident(Ident),
}

fn delimiter_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice([
        just('{').to(Token::OpenBrace),
        just('}').to(Token::CloseBrace),
        just('(').to(Token::OpenParen),
        just(')').to(Token::CloseParen),
        just(',').to(Token::Comma),
        just(';').to(Token::Semi),
    ])
}

fn operator_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice([
        // arrow operator
        just("=>").to(Token::Arrow),
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
    ])
}

fn keyword_lexer() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice([
        text::keyword("let").to(Token::Let),
        text::keyword("mut").to(Token::Mut),
        text::keyword("fn").to(Token::Fn),
        text::keyword("if").to(Token::If),
        text::keyword("else").to(Token::Else),
        text::keyword("while").to(Token::While),
        text::keyword("loop").to(Token::Loop),
        text::keyword("match").to(Token::Match),
        text::keyword("return").to(Token::Return),
        text::keyword("break").to(Token::Break),
        text::keyword("continue").to(Token::Continue),
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
        .map(|c| c as _)
        .map(Token::Int)
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
