use {
    chumsky::prelude::*,
    derive_more::{Display, From},
};

// TODO: Use a &str instead of a String
pub type Ident = String;

#[derive(Debug, Clone)]
pub enum Place {
    Var(Ident),
    Deref(Ident),
}

// TODO: Get rid of SimpleExpr and just use Expr.
#[derive(Debug, Clone)]
pub enum SimpleExpr {
    Int(usize),
    String(String),
    Place(Place),
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    // TODO: Instead of an Ident for the func, take an Expr to allow for dynamic dispatch.
    pub func: Ident,
    pub bang: bool,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Simple(SimpleExpr),
    Call(CallExpr),
}

#[derive(Debug, Clone, Copy)]
pub enum AssignMode {
    Add,
    Subtract,
    Replace,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub mutable: bool,
    pub name: Ident,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        decl: VarDecl,
        value: Expr,
    },
    Assign {
        place: Place,
        value: Expr,
        mode: AssignMode,
    },
    Loop(Vec<Statement>),
    Continue,
    Break,
    Switch {
        cond: Expr,
        cases: Vec<(usize, Vec<Statement>)>,
        default: Vec<Statement>,
    },
    Block(Vec<Statement>),
    Return(Expr),
    Eval(Expr),
}

impl From<Statement> for Vec<Statement> {
    fn from(statement: Statement) -> Self {
        match statement {
            Statement::Block(body) => body,
            _ => Vec::from([statement]),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<VarDecl>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, From, Display)]
pub enum ParseError {
    #[display("Lex error: {_0:#?}")]
    LexError(Vec<Simple<char>>),
    #[display("Parse error: {_0:#?}")]
    ParseError(Vec<Simple<Token>>),
}

impl std::error::Error for ParseError {}

impl Ast {
    pub fn parse(src: &str) -> Result<Self, ParseError> {
        let tokens = lexer().parse(src)?;
        let ast = parser().parse(tokens)?;
        Ok(ast)
    }
}

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

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let delimiter = choice([
        just('{').to(Token::OpenBrace),
        just('}').to(Token::CloseBrace),
        just('(').to(Token::OpenParen),
        just(')').to(Token::CloseParen),
        just(',').to(Token::Comma),
        just(';').to(Token::Semi),
    ]);

    let operator = choice([
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
    ]);

    let keyword = choice([
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
    ]);

    let char_escape = just('\\').ignore_then(choice([
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('\\').to('\\'),
        just('"').to('"'),
        just('\'').to('\''),
    ]));

    let string_literal = just('"')
        .ignore_then(none_of("\"\\").or(char_escape).repeated().collect())
        .then_ignore(just('"'))
        .map(Token::String);

    let char_literal = just('\'')
        .ignore_then(none_of("'\\").or(char_escape))
        .then_ignore(just('\''))
        .map(|c| c as _)
        .map(Token::Int);

    let int_literal = text::int(10).from_str().unwrapped().map(Token::Int);

    let ident = text::ident().map(Token::Ident);

    let token = choice((
        delimiter,
        operator,
        keyword,
        string_literal,
        char_literal,
        int_literal,
        ident,
    ));

    let comment = just("//").then(none_of('\n').repeated()).padded();

    token
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}

#[allow(clippy::let_and_return)]
fn parser() -> impl Parser<Token, Ast, Error = Simple<Token>> {
    let ident = select! { Token::Ident(ident) => ident };

    let r#deref = just(Token::Star).ignore_then(ident).map(Place::Deref);

    let place = ident.map(Place::Var).or(r#deref);

    let int = select! { Token::Int(int) => int };

    let string = select! { Token::String(string) => string };

    let simple_expr = place
        .clone()
        .map(SimpleExpr::Place)
        .or(int.map(SimpleExpr::Int))
        .or(string.map(SimpleExpr::String))
        .map(Expr::Simple);

    let expr = recursive(|expr| {
        let call = ident
            .then(just(Token::Bang).or_not().map(|i| i.is_some()))
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            .map(|((func, bang), args)| Expr::Call(CallExpr { func, bang, args }));

        let atom = call
            .or(simple_expr)
            .or(expr.delimited_by(just(Token::OpenParen), just(Token::CloseParen)));

        let prec0 = just(Token::Bang)
            .to("!")
            .repeated()
            .then(atom)
            .foldr(|op, expr| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![expr],
                })
            });

        let prec1 = prec0
            .clone()
            .then(
                (choice([
                    just(Token::Star).to("*"),
                    just(Token::Slash).to("/"),
                    just(Token::Percent).to("%"),
                ]))
                .then(prec0)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec2 = prec1
            .clone()
            .then(
                (choice([just(Token::Plus).to("+"), just(Token::Minus).to("-")]))
                    .then(prec1)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec3 = prec2
            .clone()
            .then(
                (choice([
                    just(Token::LtEq).to("<="),
                    just(Token::Lt).to("<"),
                    just(Token::GtEq).to(">="),
                    just(Token::Gt).to(">"),
                    just(Token::EqEq).to("=="),
                    just(Token::BangEq).to("!="),
                ]))
                .then(prec2)
                .or_not(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec4 = prec3
            .clone()
            .then(just(Token::AndAnd).to("&&").then(prec3).repeated())
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: true,
                    args: vec![lhs, rhs],
                })
            });

        let prec5 = prec4
            .clone()
            .then(just(Token::OrOr).to("||").then(prec4).repeated())
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: true,
                    args: vec![lhs, rhs],
                })
            });

        prec5
    });

    let maybe_mut = just(Token::Mut).or_not().map(|m| m.is_some());

    let r#let = just(Token::Let)
        .ignore_then(maybe_mut.clone())
        .then(ident)
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map(|((mutable, name), value)| Statement::Let {
            decl: VarDecl { mutable, name },
            value,
        });

    let primitive_assign = place
        .clone()
        .then(choice([
            just(Token::PlusEq).to(AssignMode::Add),
            just(Token::MinusEq).to(AssignMode::Subtract),
            just(Token::Eq).to(AssignMode::Replace),
        ]))
        .then(expr.clone())
        .map(|((place, mode), value)| Statement::Assign { place, value, mode });

    let arithmetic_assign = place
        .clone()
        .then(choice([
            just(Token::StarEq).to("*"),
            just(Token::SlashEq).to("/"),
            just(Token::PercentEq).to("%"),
        ]))
        .then(expr.clone())
        .map(|((place, op), value)| Statement::Assign {
            place: place.clone(),
            value: Expr::Call(CallExpr {
                func: op.to_string(),
                bang: false,
                args: vec![Expr::Simple(SimpleExpr::Place(place)), value],
            }),
            mode: AssignMode::Replace,
        });

    #[derive(Clone)]
    enum ShortCircuitOp {
        And,
        Or,
    }

    let short_circuit_assign = place
        .then(choice([
            just(Token::AndAndEq).to(ShortCircuitOp::And),
            just(Token::OrOrEq).to(ShortCircuitOp::Or),
        ]))
        .then(expr.clone())
        .map(|((place, op), value)| {
            let [short_circuit, long_circuit] = [
                vec![],
                vec![Statement::Assign {
                    place: place.clone(),
                    value,
                    mode: AssignMode::Replace,
                }],
            ];
            let [false_case, true_case] = match op {
                ShortCircuitOp::And => [short_circuit, long_circuit],
                ShortCircuitOp::Or => [long_circuit, short_circuit],
            };
            Statement::Switch {
                cond: Expr::Simple(SimpleExpr::Place(place)),
                cases: vec![(0, false_case)],
                default: true_case,
            }
        });

    let assign = choice((primitive_assign, arithmetic_assign, short_circuit_assign));

    let r#return = just(Token::Return)
        .ignore_then(expr.clone())
        .map(Statement::Return);

    let r#break = just(Token::Break).to(Statement::Break);

    let r#continue = just(Token::Continue).to(Statement::Continue);

    let eval = expr.clone().map(Statement::Eval);

    let statement_without_block = choice((r#let, assign, r#return, r#break, r#continue, eval));

    let block = recursive(|block| {
        let if_else = recursive(|if_else| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(choice((
                            if_else.map(|statement| vec![statement]),
                            block.clone(),
                        )))
                        .or_not(),
                )
                .map(|((cond, main_body), else_body)| Statement::Switch {
                    cond,
                    cases: vec![(0, else_body.unwrap_or_default())],
                    default: main_body,
                })
        });

        let r#loop = just(Token::Loop)
            .ignore_then(block.clone())
            .map(Statement::Loop);

        let r#while = just(Token::While)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond, body)| {
                Statement::Loop(vec![Statement::Switch {
                    cond,
                    cases: vec![(0, vec![Statement::Break])],
                    default: body,
                }])
            });

        let statement_with_block = recursive(|statement_with_block| {
            let arm = just(Token::Arrow).ignore_then(
                statement_without_block
                    .clone()
                    .then_ignore(just(Token::Comma))
                    .or(statement_with_block.then_ignore(just(Token::Comma).or_not()))
                    .map(Vec::from),
            );

            let switch = just(Token::Match)
                .ignore_then(expr)
                .then(
                    int.then(arm.clone())
                        .repeated()
                        .then(
                            select! { Token::Ident(ident) if ident == "_" => ident }
                                .ignore_then(arm)
                                .or_not()
                                .map(Option::unwrap_or_default),
                        )
                        .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
                )
                .map(|(cond, (cases, default))| Statement::Switch {
                    cond,
                    cases,
                    default,
                });

            let statement_with_block = choice((
                if_else,
                r#loop,
                r#while,
                switch,
                block.map(Statement::Block),
            ));

            statement_with_block
        });

        let statement = statement_without_block
            .then_ignore(just(Token::Semi))
            .or(statement_with_block);

        let block = statement
            .separated_by(just(Token::Semi).repeated())
            .allow_trailing()
            .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace));

        block
    });

    let param = maybe_mut
        .then(ident)
        .map(|(mutable, name)| VarDecl { mutable, name });

    let function = just(Token::Fn)
        .ignore_then(ident)
        .then(
            param
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        )
        .then(block)
        .map(|((name, params), body)| Function { name, params, body });

    let ast = function
        .repeated()
        .then_ignore(end())
        .map(|functions| Ast { functions });

    ast
}
