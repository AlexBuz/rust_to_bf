use {
    crate::{ast::*, lexer::Token},
    chumsky::prelude::*,
};

fn ident_parser() -> impl Parser<Token, Ident, Error = Simple<Token>> + Clone {
    select! { Token::Ident(ident) => ident }
}

fn place_parser() -> impl Parser<Token, Place, Error = Simple<Token>> + Clone {
    let field_ident = ident_parser()
        .map(FieldIdent::Named)
        .or(int_parser().map(FieldIdent::Index));

    let path = ident_parser()
        .then(just(Token::Dot).ignore_then(field_ident).repeated())
        .map(|(root, trail)| Path { root, trail });

    let deref = just(Token::Star)
        .ignore_then(path.clone())
        .map(Place::Deref);

    let place_parser = path.map(Place::Path).or(deref);

    place_parser
        .clone()
        .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
        .or(place_parser)
}

fn int_parser() -> impl Parser<Token, usize, Error = Simple<Token>> + Clone {
    select! { Token::Int(int) => int }
}

fn string_parser() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::String(string) => string }
}

fn simple_expr_parser() -> impl Parser<Token, SimpleExpr, Error = Simple<Token>> + Clone {
    let ref_expr = just(Token::And)
        .ignore_then(maybe_token(Token::Mut))
        .or_not()
        .then(place_parser())
        .map(|(r#ref, place)| match r#ref {
            Some(mutable) => SimpleExpr::AddrOf { mutable, place },
            None => SimpleExpr::Place(place),
        });

    choice((
        place_parser().map(SimpleExpr::Place),
        int_parser().map(SimpleExpr::Int),
        string_parser().map(SimpleExpr::String),
        ref_expr,
    ))
}

fn tuple_parser<I, P>(item_parser: P) -> impl Parser<Token, Vec<I>, Error = Simple<Token>> + Clone
where
    I: Clone,
    P: Parser<Token, I, Error = Simple<Token>> + Clone,
{
    choice((
        // Empty tuple
        just(Token::OpenParen)
            .ignore_then(just(Token::CloseParen))
            .to(vec![]),
        // Single-item tuple
        just(Token::OpenParen)
            .ignore_then(item_parser.clone())
            .then_ignore(just(Token::Comma))
            .then_ignore(just(Token::CloseParen))
            .map(|x| vec![x]),
        // Multi-item tuple
        item_parser
            .separated_by(just(Token::Comma))
            .at_least(2)
            .allow_trailing()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
    ))
}

fn expr_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(move |expr| {
        let call_expr = ident_parser()
            .then(just(Token::Bang).or_not().map(|i| i.is_some()))
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            .map(|((func, bang), args)| Expr::Call(CallExpr { func, bang, args }));

        let struct_expr = ident_parser()
            .then(
                ident_parser()
                    .then(just(Token::Colon).ignore_then(expr.clone()).or_not())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .map(|(name, fields)| {
                Expr::Struct(StructExpr {
                    name,
                    fields: fields
                        .into_iter()
                        .map(|(name, value)| Field {
                            value: value.unwrap_or_else(|| {
                                Expr::Simple(SimpleExpr::Place(Place::Path(Path {
                                    root: name.clone(),
                                    trail: vec![],
                                })))
                            }),
                            name,
                        })
                        .collect(),
                })
            });

        let prec = choice((
            call_expr,
            struct_expr,
            tuple_parser(expr.clone()).map(Expr::Tuple),
            simple_expr_parser().map(Expr::Simple),
            expr.delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        ));

        let prec = just(Token::Bang)
            .to("!")
            .repeated()
            .then(prec)
            .foldr(|op, expr| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![expr],
                })
            });

        let prec = prec
            .clone()
            .then(
                (choice([
                    just(Token::Star).to("*"),
                    just(Token::Slash).to("/"),
                    just(Token::Percent).to("%"),
                ]))
                .then(prec)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec = prec
            .clone()
            .then(
                (choice([just(Token::Plus).to("+"), just(Token::Minus).to("-")]))
                    .then(prec)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec = prec
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
                .then(prec)
                .or_not(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec = prec
            .clone()
            .then(just(Token::AndAnd).to("&&").then(prec).repeated())
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: true,
                    args: vec![lhs, rhs],
                })
            });

        prec.clone()
            .then(just(Token::OrOr).to("||").then(prec).repeated())
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: true,
                    args: vec![lhs, rhs],
                })
            })
    })
}

fn maybe_token(token: Token) -> impl Parser<Token, bool, Error = Simple<Token>> + Clone {
    just(token).or_not().map(|m| m.is_some())
}

fn statement_without_block_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> + Clone
{
    let r#let = just(Token::Let)
        .ignore_then(maybe_token(Token::Mut))
        .then(ident_parser())
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .then(just(Token::Eq).ignore_then(expr_parser()).or_not())
        .map(|(((mutable, name), ty), value)| Statement::Let {
            mutable,
            name,
            ty,
            value,
        });

    let primitive_assign = place_parser()
        .then(choice([
            just(Token::PlusEq).to(AssignMode::Add),
            just(Token::MinusEq).to(AssignMode::Subtract),
            just(Token::Eq).to(AssignMode::Replace),
        ]))
        .then(expr_parser())
        .map(|((place, mode), value)| Statement::Assign { place, value, mode });

    let arithmetic_assign = place_parser()
        .then(choice([
            just(Token::StarEq).to("*"),
            just(Token::SlashEq).to("/"),
            just(Token::PercentEq).to("%"),
        ]))
        .then(expr_parser())
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

    let short_circuit_assign = place_parser()
        .then(choice([
            just(Token::AndAndEq).to(ShortCircuitOp::And),
            just(Token::OrOrEq).to(ShortCircuitOp::Or),
        ]))
        .then(expr_parser())
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
        .ignore_then(expr_parser().or_not().map(Option::unwrap_or_default))
        .map(Statement::Return);

    let r#break = just(Token::Break).to(Statement::Break);

    let r#continue = just(Token::Continue).to(Statement::Continue);

    let eval = expr_parser().map(Statement::Eval);

    choice((r#let, assign, r#return, r#break, r#continue, eval))
}

fn block_parser() -> impl Parser<Token, Vec<Statement>, Error = Simple<Token>> + Clone {
    recursive(move |block| {
        let if_else = recursive(|if_else| {
            just(Token::If)
                .ignore_then(expr_parser())
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
            .ignore_then(expr_parser())
            .then(block.clone())
            .map(|(cond, body)| {
                Statement::Loop(vec![Statement::Switch {
                    cond,
                    cases: vec![(0, vec![Statement::Break])],
                    default: body,
                }])
            });

        let statement_with_block = recursive(move |statement_with_block| {
            let arm = just(Token::FatArrow).ignore_then(
                statement_without_block_parser()
                    .then_ignore(just(Token::Comma))
                    .or(statement_with_block.then_ignore(just(Token::Comma).or_not()))
                    .map(Vec::from),
            );

            let switch = just(Token::Match)
                .ignore_then(expr_parser())
                .then(
                    int_parser()
                        .then(arm.clone())
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

            choice((
                if_else,
                r#loop,
                r#while,
                switch,
                block.map(Statement::Block),
            ))
        });

        let statement = statement_without_block_parser()
            .then_ignore(just(Token::Semi))
            .or(statement_with_block);

        statement
            .separated_by(just(Token::Semi).repeated())
            .allow_trailing()
            .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
    })
}

fn type_parser() -> impl Parser<Token, Type, Error = Simple<Token>> + Clone {
    recursive(|type_parser| {
        just(Token::And)
            .ignore_then(maybe_token(Token::Mut))
            .or_not()
            .then(choice((
                ident_parser().map(Type::Named),
                tuple_parser(type_parser).map(Type::Tuple),
            )))
            .map(|(r#ref, ty)| match r#ref {
                Some(mutable) => Type::Reference {
                    mutable,
                    ty: Box::new(ty),
                },
                None => ty,
            })
    })
}

fn field_parser() -> impl Parser<Token, FieldDef, Error = Simple<Token>> {
    ident_parser()
        .then_ignore(just(Token::Colon))
        .then(type_parser())
        .map(|(name, ty)| FieldDef { name, ty })
}

fn struct_def_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    just(Token::Struct)
        .ignore_then(ident_parser())
        .then(
            field_parser()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
        )
        .map(|(name, fields)| Item::StructDef { name, fields })
}

fn param_parser() -> impl Parser<Token, Param, Error = Simple<Token>> {
    maybe_token(Token::Mut)
        .then(ident_parser())
        .then_ignore(just(Token::Colon))
        .then(type_parser())
        .map(|((mutable, name), ty)| Param { mutable, name, ty })
}

fn func_def_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    just(Token::Fn)
        .ignore_then(ident_parser())
        .then(
            param_parser()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        )
        .then(
            just(Token::ThinArrow)
                .ignore_then(type_parser())
                .or_not()
                .map(Option::unwrap_or_default),
        )
        .then(block_parser())
        .map(|(((name, params), ret_ty), body)| Item::FuncDef {
            name,
            params,
            ret_ty,
            body,
        })
}

fn item_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    func_def_parser().or(struct_def_parser())
}

pub fn ast_parser() -> impl Parser<Token, Ast, Error = Simple<Token>> {
    item_parser()
        .repeated()
        .map(|items| Ast { items })
        .then_ignore(end())
}
