#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::parser::grammar::{DeclParser, ExprParser, ProgramParser};
    use Expr::*;

    fn cnum(n: i64) -> Box<Expr> {
        Box::new(Expr::Number(n))
    }

    fn cbinop(op: BinOp, lh: i64, rh: i64) -> Expr {
        Expr::Prim2(op, cnum(lh), cnum(rh))
    }

    fn cid(s: &str) -> Box<Expr> {
        Box::new(Expr::Id(s.to_string()))
    }

    #[test]
    fn test_number() {
        assert_eq!(ExprParser::new().parse("412").unwrap(), Number(412));
        assert_eq!(ExprParser::new().parse("-123").unwrap(), Number(-123));
    }

    #[test]
    fn test_ident() {
        assert_eq!(
            ExprParser::new().parse("blah0a1"),
            Ok(Id("blah0a1".to_string()))
        );
        assert!(ExprParser::new().parse("1lar").is_err());
    }

    #[test]
    fn test_prim1_expr() {
        assert_eq!(
            ExprParser::new().parse("add1(5)"),
            Ok(Expr::Prim1(MonOp::Add1, Box::new(Expr::Number(5))))
        );
        assert_eq!(
            ExprParser::new().parse("add1 ( 5 )"),
            Ok(Expr::Prim1(MonOp::Add1, Box::new(Expr::Number(5))))
        );
    }

    #[test]
    fn test_neg() {
        assert_eq!(
            ExprParser::new().parse("-x"),
            Ok(Expr::Prim1(MonOp::Neg, cid("x")))
        );
    }

    #[test]
    fn test_const_expr() {
        // number expressions
        assert_eq!(ExprParser::new().parse("-124"), Ok(Expr::Number(-124)));
    }

    #[test]
    fn test_monop() {
        // monop expressions
        assert_eq!(
            ExprParser::new().parse("add1(hello)"),
            Ok(Expr::Prim1(
                MonOp::Add1,
                Box::new(Expr::Id("hello".to_string()))
            ))
        );
    }

    #[test]
    fn test_binop() {
        // binop expressions
        assert_eq!(
            ExprParser::new().parse("1+2"),
            Ok(cbinop(BinOp::Plus, 1, 2))
        );

        assert_eq!(
            ExprParser::new().parse("1 + 2"),
            Ok(cbinop(BinOp::Plus, 1, 2))
        );

        assert_eq!(
            ExprParser::new().parse("1-2+3"),
            Ok(Expr::Prim2(
                BinOp::Plus,
                Box::new(cbinop(BinOp::Minus, 1, 2)),
                cnum(3)
            ))
        );

        assert_eq!(
            ExprParser::new().parse("1*2 - 4"),
            Ok(Expr::Prim2(
                BinOp::Minus,
                Box::new(cbinop(BinOp::Times, 1, 2)),
                cnum(4)
            ))
        );

        assert_eq!(
            ExprParser::new().parse("2/4"),
            Ok(cbinop(BinOp::Divide, 2, 4))
        );

        // paren expressions
        assert_eq!(
            ExprParser::new().parse("(1 + 2)"),
            Ok(cbinop(BinOp::Plus, 1, 2))
        );
        assert_eq!(
            ExprParser::new().parse("(   1 + 2   )"),
            Ok(cbinop(BinOp::Plus, 1, 2))
        );

        assert_eq!(
            ExprParser::new().parse("1* (2 - 4)"),
            Ok(Expr::Prim2(
                BinOp::Times,
                cnum(1),
                Box::new(cbinop(BinOp::Minus, 2, 4))
            ))
        );

        // nesting
        assert_eq!(
            ExprParser::new().parse("add1(1 + 2)"),
            Ok(Expr::Prim1(
                MonOp::Add1,
                Box::new(cbinop(BinOp::Plus, 1, 2))
            ))
        );

        assert_eq!(
            ExprParser::new().parse("5 * add1(1 + 2)"),
            Ok(Expr::Prim2(
                BinOp::Times,
                cnum(5),
                Box::new(Expr::Prim1(
                    MonOp::Add1,
                    Box::new(cbinop(BinOp::Plus, 1, 2))
                )),
            ))
        );

        assert_eq!(
            ExprParser::new().parse("1 % 2"),
            Ok(cbinop(BinOp::Mod, 1, 2))
        );

        assert_eq!(
            ExprParser::new().parse("5 * 1 % 2"),
            Ok(Expr::Prim2(
                BinOp::Mod,
                Box::new(cbinop(BinOp::Times, 5, 1)),
                cnum(2)
            ))
        );
    }

    #[test]
    fn test_let_bindings() {
        // let bindings
        assert_eq!(
            ExprParser::new().parse("let x = 5 in x"),
            Ok(Expr::Let(
                vec![("x".to_string(), Expr::Number(5))],
                cid("x")
            ))
        );

        assert_eq!(
            ExprParser::new().parse("let x = 5, y = x + 3 in x"),
            Ok(Expr::Let(
                vec![
                    ("x".to_string(), Expr::Number(5)),
                    ("y".to_string(), Expr::Prim2(BinOp::Plus, cid("x"), cnum(3)))
                ],
                Box::new(Expr::Id("x".to_string()))
            ))
        );

        assert_eq!(
            ExprParser::new().parse("3 * (let x = 5, y = x + 3 in x)"),
            Ok(Expr::Prim2(
                BinOp::Times,
                cnum(3),
                Box::new(Expr::Let(
                    vec![
                        ("x".to_string(), Expr::Number(5)),
                        ("y".to_string(), Expr::Prim2(BinOp::Plus, cid("x"), cnum(3)))
                    ],
                    Box::new(Expr::Id("x".to_string()))
                ))
            ))
        );
    }

    #[test]
    fn test_comparisons() {
        // comparisons
        assert_eq!(
            ExprParser::new().parse("3 < (1 * 5)"),
            Ok(Expr::Prim2(
                BinOp::Less,
                cnum(3),
                Box::new(cbinop(BinOp::Times, 1, 5))
            ))
        );
        assert_eq!(
            ExprParser::new().parse("3 < 1 * 5"),
            Ok(Expr::Prim2(
                BinOp::Less,
                cnum(3),
                Box::new(cbinop(BinOp::Times, 1, 5))
            ))
        );
    }

    #[test]
    fn test_conditionals() {
        // if
        assert_eq!(
            ExprParser::new().parse("if true: 5 else: 10"),
            Ok(Expr::If(Box::new(Expr::Bool(true)), cnum(5), cnum(10)))
        );

        assert_eq!(
            ExprParser::new().parse("let x = 30 in if 5 + 10 < 30: add1(x) else: 5"),
            Ok(Expr::Let(
                vec![("x".to_string(), Expr::Number(30))],
                Box::new(Expr::If(
                    Box::new(Expr::Prim2(
                        BinOp::Less,
                        Box::new(Expr::Prim2(
                            BinOp::Plus,
                            Box::new(Expr::Number(5)),
                            Box::new(Expr::Number(10))
                        )),
                        Box::new(Expr::Number(30))
                    )),
                    Box::new(Expr::Prim1(
                        MonOp::Add1,
                        Box::new(Expr::Id("x".to_string()))
                    )),
                    Box::new(Expr::Number(5))
                ))
            ))
        );
    }

    #[test]
    fn test_application() {
        assert_eq!(
            ExprParser::new().parse("hello(1)"),
            Ok(Expr::App(cid("hello"), vec![Expr::Number(1),]))
        );

        assert_eq!(
            ExprParser::new().parse("hello(arg1, 1 + 2)"),
            Ok(Expr::App(
                cid("hello"),
                vec![Expr::Id("arg1".to_string()), cbinop(BinOp::Plus, 1, 2),]
            ))
        );

        assert_eq!(
            ExprParser::new().parse("f(1)(arg1, 1 + 2)"),
            Ok(Expr::App(
                Box::new(Expr::App(cid("f"), vec![Number(1)])),
                vec![Expr::Id("arg1".to_string()), cbinop(BinOp::Plus, 1, 2),]
            ))
        );
    }

    #[test]
    fn test_tuples() {
        assert_eq!(
            ExprParser::new().parse("(1 ,2, 103 )"),
            Ok(Expr::Tuple(vec![Number(1), Number(2), Number(103),]))
        );

        assert_eq!(
            ExprParser::new().parse("( 1 , 2 + 3)"),
            Ok(Expr::Tuple(vec![Number(1), cbinop(BinOp::Plus, 2, 3),]))
        );

        assert_eq!(
            ExprParser::new().parse("(1, (2, true))"),
            Ok(Expr::Tuple(vec![
                Number(1),
                Expr::Tuple(vec![Number(2), Bool(true),])
            ]))
        );

        // should not be parsed as a tuple
        assert_eq!(ExprParser::new().parse("(1)"), Ok(Expr::Number(1)));
    }

    #[test]
    fn test_tuple_access() {
        assert_eq!(
            ExprParser::new().parse("x[1 ]"),
            Ok(GetItem(cid("x"), cnum(1)))
        );

        assert_eq!(
            ExprParser::new().parse("(x[1])[1]"),
            Ok(GetItem(Box::new(GetItem(cid("x"), cnum(1))), cnum(1)))
        );

        assert_eq!(
            ExprParser::new().parse("x[1][1]"),
            Ok(GetItem(Box::new(GetItem(cid("x"), cnum(1))), cnum(1)))
        );

        assert_eq!(
            ExprParser::new().parse("f(x)[f(1)]"),
            Ok(GetItem(
                Box::new(App(cid("f"), vec![Id("x".to_string())])),
                Box::new(App(cid("f"), vec![Number(1)])),
            ))
        );

        assert_eq!(
            ExprParser::new().parse("(4 * 1)[f(1)]"),
            Ok(GetItem(
                Box::new(cbinop(BinOp::Times, 4, 1)),
                Box::new(App(cid("f"), vec![Number(1)])),
            ))
        );

        assert_eq!(
            ExprParser::new().parse("x[ 1 + x[2]]"),
            Ok(GetItem(
                cid("x"),
                Box::new(Prim2(
                    BinOp::Plus,
                    cnum(1),
                    Box::new(Expr::GetItem(cid("x"), cnum(2)))
                ))
            ))
        );

        // (1, 2, 3)[1]
        assert_eq!(
            ExprParser::new().parse("(1, 2, 3)[1]"),
            Ok(GetItem(
                Box::new(Tuple(vec![Number(1), Number(2), Number(3)])),
                cnum(1)
            ))
        );
    }

    #[test]
    fn test_tuple_set() {
        assert_eq!(
            ExprParser::new().parse("t[3] = 5"),
            Ok(SetItem(cid("t"), cnum(3), cnum(5),))
        );

        assert_eq!(
            ExprParser::new().parse("(0, 1, 2)[f(1)] = 10"),
            Ok(SetItem(
                Box::new(Tuple(vec![Number(0), Number(1), Number(2)])),
                Box::new(App(cid("f"), vec![Number(1)])),
                cnum(10),
            ))
        );

        assert_eq!(
            ExprParser::new().parse("let x = y[1] = 10 in x"),
            Ok(Let(
                vec![("x".to_string(), SetItem(cid("y"), cnum(1), cnum(10),))],
                cid("x")
            ))
        );
    }

    #[test]
    fn test_begin() {
        assert_eq!(
            ExprParser::new().parse("begin\n1 + 2;\nx;\nend"),
            Ok(Block(vec![cbinop(BinOp::Plus, 1, 2), Id("x".to_string()),]))
        );

        assert_eq!(
            ExprParser::new().parse("begin 1 + (begin 5; end); end"),
            Ok(Block(vec![Prim2(
                BinOp::Plus,
                cnum(1),
                Box::new(Block(vec![Number(5)]))
            )]))
        );
    }

    #[test]
    fn test_lambda() {
        assert_eq!(
            ExprParser::new().parse(r#"(\x1 y -> x1 + y)"#),
            Ok(Lambda(
                vec!["x1".to_string(), "y".to_string()],
                Box::new(Prim2(BinOp::Plus, cid("x1"), cid("y")))
            ))
        );

        assert_eq!(
            ExprParser::new().parse(r#"(\x1 y -> x1 + y)(10, 5)"#),
            Ok(App(
                Box::new(Lambda(
                    vec!["x1".to_string(), "y".to_string()],
                    Box::new(Prim2(BinOp::Plus, cid("x1"), cid("y")))
                )),
                vec![Number(10), Number(5),]
            ))
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(
            ExprParser::new().parse("5 * 3 // this is a comment\n"),
            Ok(cbinop(BinOp::Times, 5, 3))
        );
    }

    #[test]
    fn test_decl() {
        assert_eq!(
            DeclParser::new().parse("def test( arg1 , arg2 ) { arg1 + 1 }"),
            Ok(Decl {
                name: "test".to_string(),
                params: vec!["arg1".to_string(), "arg2".to_string()],
                body: Expr::Prim2(
                    BinOp::Plus,
                    Box::new(Expr::Id("arg1".to_string())),
                    Box::new(Expr::Number(1))
                )
            })
        );
    }

    #[test]
    fn test_program() {
        assert_eq!(
            ProgramParser::new().parse("def f(x) { x + 1 }\ndef g(x) { g(x) }\nf(1)"),
            Ok(Program {
                decls: vec![
                    Decl {
                        name: "f".to_string(),
                        params: vec!["x".to_string()],
                        body: Expr::Prim2(
                            BinOp::Plus,
                            Box::new(Expr::Id("x".to_string())),
                            cnum(1)
                        ),
                    },
                    Decl {
                        name: "g".to_string(),
                        params: vec!["x".to_string()],
                        body: Expr::App(cid("g"), vec![Expr::Id("x".to_string())]),
                    }
                ],
                init: Expr::App(cid("f"), vec![Expr::Number(1)])
            })
        );
    }
}
