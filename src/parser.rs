use {
    crate::{ast::*, lexer::Token},
    chumsky::prelude::*,
};

fn ident_parser() -> impl Parser<Token, Ident, Error = Simple<Token>> + Clone {
    select! { Token::Ident(ident) => ident }
}

fn place_parser(
    atom_parser: impl Parser<Token, Expr, Error = Simple<Token>> + Clone + 'static,
) -> impl Parser<Token, Place, Error = Simple<Token>> + Clone {
    enum FieldOrIndex {
        Field(Ident),
        Index(Expr),
    }

    recursive(|place_parser| {
        choice((
            just(Token::Star)
                .ignore_then(atom_parser.clone())
                .map(Box::new)
                .map(Place::Deref),
            ident_parser().map(Place::Var),
            place_parser.delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        ))
        .then(
            choice((
                just(Token::Dot)
                    .ignore_then(ident_parser().or(select! { Token::Int(int) => int }))
                    .map(FieldOrIndex::Field),
                just(Token::OpenBracket)
                    .ignore_then(atom_parser) // TODO: allow this to be an arbitrary expression
                    .then_ignore(just(Token::CloseBracket))
                    .map(FieldOrIndex::Index),
            ))
            .repeated(),
        )
        .foldl(|base, field_or_index| match field_or_index {
            FieldOrIndex::Field(field) => Place::FieldAccess {
                base: Box::new(base),
                field,
            },
            FieldOrIndex::Index(index) => Place::Index {
                base: Box::new(base),
                index: Box::new(index),
            },
        })
    })
}

fn int_parser() -> impl Parser<Token, usize, Error = Simple<Token>> + Clone {
    select! { Token::Int(int) => int }.from_str().unwrapped()
}

fn char_parser() -> impl Parser<Token, char, Error = Simple<Token>> + Clone {
    select! { Token::Char(c) => c }
}

fn bool_parser() -> impl Parser<Token, bool, Error = Simple<Token>> + Clone {
    select! {
        Token::True => true,
        Token::False => false,
    }
}

fn string_parser() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::String(string) => string }
}

fn tuple_parser<Elem: Clone>(
    elem_parser: impl Parser<Token, Elem, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Vec<Elem>, Error = Simple<Token>> + Clone {
    choice((
        // multiple elements
        elem_parser
            .clone()
            .separated_by(just(Token::Comma))
            .at_least(2)
            .allow_trailing(),
        // single element
        elem_parser.then_ignore(just(Token::Comma)).map(|x| vec![x]),
        // empty
        empty().to(vec![]),
    ))
    .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
}

fn repeat_parser<Elem>(
    elem_parser: impl Parser<Token, Elem, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, (Box<Elem>, usize), Error = Simple<Token>> + Clone {
    elem_parser
        .map(Box::new)
        .then_ignore(just(Token::Semi))
        .then(int_parser())
        .delimited_by(just(Token::OpenBracket), just(Token::CloseBracket))
}

fn atom_parser(
    expr_parser: impl Parser<Token, Expr, Error = Simple<Token>> + Clone + 'static,
    deny_top_level_empty_struct: bool,
) -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(|atom_parser| {
        let ref_expr = just(Token::And)
            .ignore_then(maybe_token(Token::Mut))
            .or_not()
            .then(place_parser(atom_parser.clone()))
            .map(|(r#ref, place)| match r#ref {
                Some(mutable) => Expr::Ref { mutable, place },
                None => Expr::Place(place),
            });

        let call_expr = ident_parser()
            .then(just(Token::Bang).or_not().map(|i| i.is_some()))
            .then(
                expr_parser
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            .map(|((func, bang), args)| Expr::Call(CallExpr { func, bang, args }));

        let struct_expr = |min_fields| {
            ident_parser()
                .then(
                    ident_parser()
                        .then(just(Token::Colon).ignore_then(expr_parser.clone()).or_not())
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .at_least(min_fields)
                        .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
                )
                .map(|(name, fields)| {
                    Expr::Struct(StructExpr {
                        name,
                        fields: fields
                            .into_iter()
                            .map(|(name, value)| Field {
                                value: value
                                    .unwrap_or_else(|| Expr::Place(Place::Var(name.clone()))),
                                name,
                            })
                            .collect(),
                    })
                })
        };

        let array_expr = choice((
            expr_parser
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::OpenBracket), just(Token::CloseBracket))
                .map(ArrayExpr::List),
            repeat_parser(expr_parser.clone()).map(|(value, len)| ArrayExpr::Repeat { value, len }),
        ))
        .map(Expr::Array);

        choice((
            call_expr,
            struct_expr(deny_top_level_empty_struct as usize),
            tuple_parser(expr_parser.clone()).map(Expr::Tuple),
            array_expr,
            place_parser(atom_parser).map(Expr::Place),
            int_parser().map(Expr::Int),
            char_parser().map(Expr::Char),
            bool_parser().map(Expr::Bool),
            string_parser().map(Expr::String),
            ref_expr,
            (struct_expr(0).or(expr_parser))
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        ))
    })
}

fn expr_parser(
    deny_top_level_empty_struct: bool,
) -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(move |expr_parser| {
        let prec = just(Token::Bang)
            .to("!")
            .repeated()
            .then(atom_parser(expr_parser, deny_top_level_empty_struct))
            .foldr(|op, expr| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![expr],
                })
            });

        let prec = prec
            .clone()
            .then(just(Token::As).ignore_then(type_parser()).repeated())
            .foldl(|expr, ty| Expr::Cast {
                expr: Box::new(expr),
                ty,
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
                    bang: matches!(op, "==" | "!="),
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

fn pattern_parser() -> impl Parser<Token, Pattern, Error = Simple<Token>> + Clone {
    choice((
        int_parser().map(Pattern::Int),
        char_parser().map(Pattern::Char),
        bool_parser().map(Pattern::Bool),
        just(Token::Underscore).to(Pattern::Wildcard),
    ))
}

fn statement_without_block_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> + Clone
{
    let r#let = just(Token::Let)
        .ignore_then(maybe_token(Token::Mut))
        .then(ident_parser())
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .then(just(Token::Eq).ignore_then(expr_parser(false)).or_not())
        .map(|(((mutable, name), ty), value)| Statement::Let {
            mutable,
            name,
            ty,
            value,
        });

    let simple_assign = place_parser(expr_parser(false))
        .then(choice([
            just(Token::PlusEq).to(AssignMode::Add),
            just(Token::MinusEq).to(AssignMode::Subtract),
            just(Token::Eq).to(AssignMode::Replace),
        ]))
        .then(expr_parser(false))
        .map(|((place, mode), value)| Statement::Assign { place, value, mode });

    let arithmetic_assign = place_parser(expr_parser(false))
        .then(choice([
            just(Token::StarEq).to("*"),
            just(Token::SlashEq).to("/"),
            just(Token::PercentEq).to("%"),
        ]))
        .then(expr_parser(false))
        .map(|((place, op), value)| Statement::Assign {
            place: place.clone(),
            value: Expr::Call(CallExpr {
                func: op.to_string(),
                bang: false,
                args: vec![Expr::Place(place), value],
            }),
            mode: AssignMode::Replace,
        });

    #[derive(Clone)]
    enum ShortCircuitOp {
        And,
        Or,
    }

    let short_circuit_assign = place_parser(expr_parser(false))
        .then(choice([
            just(Token::AndAndEq).to(ShortCircuitOp::And),
            just(Token::OrOrEq).to(ShortCircuitOp::Or),
        ]))
        .then(expr_parser(false))
        .map(|((place, op), value)| {
            let [short_circuit, long_circuit] = [
                vec![],
                vec![Statement::Assign {
                    place: place.clone(),
                    value,
                    mode: AssignMode::Replace,
                }],
            ];
            let [false_branch, true_branch] = match op {
                ShortCircuitOp::And => [short_circuit, long_circuit],
                ShortCircuitOp::Or => [long_circuit, short_circuit],
            };
            Statement::If {
                cond: Expr::Place(place),
                true_branch,
                false_branch,
            }
        });

    let assign = choice((simple_assign, arithmetic_assign, short_circuit_assign));

    let r#return = just(Token::Return)
        .ignore_then(expr_parser(false).or_not().map(Option::unwrap_or_default))
        .map(Statement::Return);

    let r#break = just(Token::Break).to(Statement::Break);

    let r#continue = just(Token::Continue).to(Statement::Continue);

    let eval = expr_parser(false).map(Statement::Eval);

    choice((r#let, assign, r#return, r#break, r#continue, eval))
}

fn block_parser() -> impl Parser<Token, Vec<Statement>, Error = Simple<Token>> + Clone {
    recursive(move |block_parser| {
        let if_else = recursive(|if_else| {
            just(Token::If)
                .ignore_then(expr_parser(true))
                .then(block_parser.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(choice((
                            if_else.map(|statement| vec![statement]),
                            block_parser.clone(),
                        )))
                        .or_not()
                        .map(Option::unwrap_or_default),
                )
                .map(|((cond, true_branch), false_branch)| Statement::If {
                    cond,
                    true_branch,
                    false_branch,
                })
        });

        let r#loop = just(Token::Loop)
            .ignore_then(block_parser.clone())
            .map(Statement::Loop);

        let r#while = just(Token::While)
            .ignore_then(expr_parser(true))
            .then(block_parser.clone())
            .map(|(cond, body)| {
                Statement::Loop(vec![Statement::If {
                    cond,
                    true_branch: body,
                    false_branch: vec![Statement::Break],
                }])
            });

        let statement_with_block = recursive(move |statement_with_block| {
            let r#match = just(Token::Match)
                .ignore_then(expr_parser(true))
                .then(
                    pattern_parser()
                        .then(
                            just(Token::FatArrow).ignore_then(
                                statement_without_block_parser()
                                    .then_ignore(just(Token::Comma))
                                    .or(statement_with_block
                                        .then_ignore(just(Token::Comma).or_not()))
                                    .map(Vec::from),
                            ),
                        )
                        .repeated()
                        .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
                )
                .map(|(scrutinee, arms)| Statement::Match { scrutinee, arms });

            choice((
                if_else,
                r#loop,
                r#while,
                r#match,
                block_parser.map(Statement::Block),
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
            .repeated()
            .then(choice((
                ident_parser().map(Type::Named),
                tuple_parser(type_parser.clone()).map(Type::Tuple),
                repeat_parser(type_parser.clone()).map(|(ty, len)| Type::Array { ty, len }),
                type_parser.delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )))
            .foldr(|mutable, ty| Type::Ref {
                mutable,
                ty: Box::new(ty),
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
