use {
    chumsky::prelude::{Parser as ChumskyParser, *},
    derive_more::Display,
};

pub(super) trait Parser<'src, Output>:
    ChumskyParser<'src, &'src str, Output, extra::Err<Rich<'src, char>>> + Clone
{
}
impl<
        'src,
        Output,
        T: ChumskyParser<'src, &'src str, Output, extra::Err<Rich<'src, char>>> + Clone,
    > Parser<'src, Output> for T
{
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum Token<'src> {
    // delimiters
    #[display("{{")]
    OpenBrace,
    #[display("}}")]
    CloseBrace,
    #[display("[")]
    OpenBracket,
    #[display("]")]
    CloseBracket,
    #[display("(")]
    OpenParen,
    #[display(")")]
    CloseParen,
    #[display(",")]
    Comma,
    #[display(".")]
    Dot,
    #[display(";")]
    Semi,
    #[display(":")]
    Colon,
    // arrows
    #[display("->")]
    ThinArrow,
    #[display("=>")]
    FatArrow,
    // relational operators
    #[display("<=")]
    LtEq,
    #[display("<")]
    Lt,
    #[display(">=")]
    GtEq,
    #[display(">")]
    Gt,
    #[display("==")]
    EqEq,
    #[display("!=")]
    BangEq,
    // assignment operators
    #[display("+=")]
    PlusEq,
    #[display("-=")]
    MinusEq,
    #[display("*=")]
    StarEq,
    #[display("/=")]
    SlashEq,
    #[display("%=")]
    PercentEq,
    #[display("=")]
    Eq,
    // logical operators
    #[display("&&")]
    AndAnd,
    #[display("||")]
    OrOr,
    #[display("!")]
    Bang,
    // arithmetic operators
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Star,
    #[display("/")]
    Slash,
    #[display("%")]
    Percent,
    // references
    #[display("&")]
    And,
    // keywords
    #[display("as")]
    As,
    #[display("let")]
    Let,
    #[display("mut")]
    Mut,
    #[display("fn")]
    Fn,
    #[display("struct")]
    Struct,
    #[display("if")]
    If,
    #[display("else")]
    Else,
    #[display("while")]
    While,
    #[display("loop")]
    Loop,
    #[display("match")]
    Match,
    #[display("return")]
    Return,
    #[display("break")]
    Break,
    #[display("continue")]
    Continue,
    // literals
    #[display("true")]
    True,
    #[display("false")]
    False,
    Int(&'src str),
    #[display("'{_0}'")]
    Char(char),
    #[display("\"{_0}\"")]
    Str(&'src str),
    // identifiers
    Ident(&'src str),
    #[display("_")]
    Underscore,
}

fn delimiter_lexer<'src>() -> impl Parser<'src, Token<'src>> {
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

fn operator_lexer<'src>() -> impl Parser<'src, Token<'src>> {
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

fn char_escape_lexer<'src>() -> impl Parser<'src, char> {
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

fn string_literal_lexer<'src>() -> impl Parser<'src, Token<'src>> {
    none_of("\"\\")
        .or(char_escape_lexer())
        .repeated()
        .to_slice()
        .delimited_by(just('"'), just('"'))
        .map(Token::Str)
}

fn char_literal_lexer<'src>() -> impl Parser<'src, Token<'src>> {
    just('\'')
        .ignore_then(none_of("'\\").or(char_escape_lexer()))
        .then_ignore(just('\''))
        .map(Token::Char)
}

fn int_literal_lexer<'src>() -> impl Parser<'src, Token<'src>> {
    text::int(10).map(Token::Int)
}

fn ident_lexer<'src>() -> impl Parser<'src, Token<'src>> {
    text::ascii::ident().map(|ident| match ident {
        "as" => Token::As,
        "let" => Token::Let,
        "mut" => Token::Mut,
        "fn" => Token::Fn,
        "struct" => Token::Struct,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "loop" => Token::Loop,
        "match" => Token::Match,
        "return" => Token::Return,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "true" => Token::True,
        "false" => Token::False,
        "_" => Token::Underscore,
        _ => Token::Ident(ident),
    })
}

fn token_lexer<'src>() -> impl Parser<'src, Token<'src>> {
    choice((
        delimiter_lexer(),
        operator_lexer(),
        string_literal_lexer(),
        char_literal_lexer(),
        int_literal_lexer(),
        ident_lexer(),
    ))
}

fn comment_lexer<'src>() -> impl Parser<'src, ()> {
    just("//").then(none_of('\n').repeated()).padded().ignored()
}

pub(super) fn lexer<'src>() -> impl Parser<'src, Vec<Token<'src>>> {
    token_lexer()
        .padded_by(comment_lexer().repeated())
        .padded()
        .repeated()
        .collect()
        .then_ignore(end())
}
