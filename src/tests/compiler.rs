#[cfg(test)]
mod tests {
    use crate::ast::Decl;
    use crate::ast::Expr::Number;
    use crate::compiler::Compiler;
    use crate::parser;
    use crate::tester::*;
    use std::collections::HashSet;

    #[test]
    fn test_constant() {
        test_success("10", 0, "10\n");
    }

    #[test]
    fn test_mono() {
        test_success("add1(1)", 0, "2\n");
    }

    #[test]
    fn test_arithmetic() {
        test_success("9 + (3 - 7) * 2", 0, "1\n");
    }

    #[test]
    fn test_arithmetic2() {
        test_success("(5 - 10 + 11) * 5", 0, "30\n");
    }

    #[test]
    fn test_arithmetic3() {
        test_success("(5 - 10 + 11) + 5", 0, "11\n");
    }

    #[test]
    fn test_arithmetic4() {
        test_success("5 + 3", 0, "8\n");
    }

    #[test]
    fn test_arithmetic5() {
        test_success("1 - 2", 0, "-1\n");
    }

    #[test]
    fn test_arithmetic6() {
        test_success("7 % 3", 0, "1\n");
        // TODO: I don't like this behavior
        test_success("-7 % 3", 0, "-1\n");
    }

    #[test]
    fn test_arithmetic7() {
        test_success("4 / 2", 0, "2\n");
        test_success("5 / 2", 0, "2\n");
    }

    #[test]
    fn test_neg() {
        test_success("-(4 + 7)", 0, "-11\n");
    }

    #[test]
    fn test_complex() {
        let program = r#"
let x = 5 in
  let y = 10, z = x * 2 + 1 in
    add1((x - y + z) * 5)
        "#;

        test_success(program, 0, "31\n");
    }

    #[test]
    fn test_let() {
        let program = "let x = 5 in let y = x * 2 in y * 2";
        test_success(program, 0, "20\n");
    }

    #[test]
    fn test_let_shadowing() {
        test_success("let x = 5 in let x = 13 in x", 0, "13\n");
    }

    #[test]
    fn test_cmp() {
        test_success("15 < 10 + 20", 0, "true\n");
        test_success("35 < 10 + 20", 0, "false\n");
    }

    #[test]
    fn test_type_checks() {
        test_success("isNum(5 + 5)", 0, "true\n");
        test_success("isNum(true)", 0, "false\n");
        test_success("isNum(5 < 10)", 0, "false\n");

        test_success("isBool(5 < 10)", 0, "true\n");
        test_success("isBool(false)", 0, "true\n");
        test_success("isBool(5 * 10)", 0, "false\n");
        test_success("isBool((5, 9))", 0, "false\n");

        test_success("isTuple((1,2,3))", 0, "true\n");
        test_success("isTuple(false)", 0, "false\n");
    }

    #[test]
    fn test_if() {
        test_success("if true: 10 else: 50", 0, "10\n");
    }

    #[test]
    fn test_checks() {
        test_error("x + 5", 2, 0, "", "");
        test_error("let x = 5, y=5, x=10 in x + y", 2, 0, "", "");
        test_error("let x = blah(5) in x", 2, 0, "", "");
    }

    #[test]
    fn test_int_check() {
        test_error("add1(true)", 0, 1, "", "expected a number\n");
        test_error("1 + true", 0, 1, "", "expected a number\n");
        test_error("(1 < 10) + 5", 0, 1, "", "expected a number\n")
    }

    #[test]
    fn test_bool_check() {
        test_error("if 1: 1 else: 0", 0, 2, "", "expected a bool\n");
    }

    #[test]
    fn test_overflow() {
        test_error("1152921504606846975 * 16", 0, 3, "", "overflow\n");
    }

    #[test]
    fn test_fn() {
        test_success("def f(x) { x + 1 }\nf(2*3)", 0, "7\n");
    }

    #[test]
    fn test_program() {
        test_success("def g(x) { 1 } def f(x) { g(x) } f(1)", 0, "1\n");
    }

    #[test]
    fn test_recursion() {
        let program = r#"
        def sum(a) {
          if a == 0: 0
          else: 1 + sum(a - 1)
        }
        sum(20)
        "#;
        test_success(program, 0, "20\n");
    }

    #[test]
    fn test_fib() {
        let program = r#"
        def fib(a) {
          if a == 0: 1
          else: if a == 1: 1
               else: fib(a - 1) + fib(a - 2)
        }
        fib(13)
        "#;
        test_success(program, 0, "377\n");
    }

    #[test]
    fn test_print() {
        test_success("print(13)", 0, "13\n13\n");

        test_success(
            "def f(x) { print(x) }\ndef g(x) { print(f(x)) }\ng(5)",
            0,
            "5\n5\n5\n",
        );

        test_success(
            "def f(x,y){ print(x * y) } def g(x){ print((f(x, x * 10)) * 2)} g(5 + 7)",
            0,
            "1440\n2880\n2880\n",
        );
    }

    #[test]
    fn test_fn_valid() {
        test_error("def f(x) { x } f(1,2)", 2, 0, "", "");
        test_error("g(1)", 2, 0, "", "");
        test_error("def f(x, x) { x }f(1,2)", 2, 0, "", "");
        test_error("def f(x) {x} def f(x) { x * 2 }\nf(1)", 2, 0, "", "");
        test_error("def f(x) { x } (f(1, 2), 5)", 2, 0, "", "");
        test_error("def f(x) { x } (1, 2)[f(1, 2)]", 2, 0, "", "");
        test_error("def f(x, y) { x + y }f(1)", 2, 0, "", "");
    }

    #[test]
    fn test_multiple_args() {
        // without TCO
        test_success("def f(x, y) { y - x } f(1, 2) + 1", 0, "2\n");
        // with TCO
        test_success("def f(x, y){ y - x } f(1, 2)", 0, "1\n");

        // without tco
        test_success(
            "def f(x, y){ if x == 0: y else: f(x-1, y+1) + 1 } f(3, 0)",
            0,
            "6\n",
        );
        // with tco
        test_success(
            "def f(x, y){ if x == 0: y else: f(x-1, y+1) } f(3, 0)",
            0,
            "3\n",
        );
    }

    #[test]
    fn test_tuple() {
        test_success("(1, 3)", 0, "(1, 3)\n");
        test_success("(1, 3, 5)", 0, "(1, 3, 5)\n");

        test_success("(1, 3, 5)[1]", 0, "3\n");

        test_success("(1, 3 * 2, 5)[2 * 7 - 13]", 0, "6\n");

        test_success("(1, 3 * 2, 5)[2 * 7 - 13]", 0, "6\n");
        test_success("(1, 3, (5, 6, false))", 0, "(1, 3, (5, 6, false))\n");
        test_success("(1, 3, (5, 6, false))[2]", 0, "(5, 6, false)\n");

        test_success(
            "let x = (1, 2, 3, 4, 5) in (x[0], x[1], x[2], x[3], x[4])",
            0,
            "(1, 2, 3, 4, 5)\n",
        );

        test_success("def f(x) { (x, x, x) } f(5)", 0, "(5, 5, 5)\n");

        test_success(
            "def f(x) { (x, x, x) } f((1, 2))",
            0,
            "((1, 2), (1, 2), (1, 2))\n",
        );

        test_success("let x = (1, 3) in x == x", 0, "true\n");
        test_success("(1, 3) == (1, 3)", 0, "false\n");
    }

    #[test]
    fn test_list() {
        test_success(
            "def idx(l, i) {
  if i == 0: l[0]
  else: idx(l[1], i - 1)
  }
  idx((1, (2, (3, false))), 1)",
            0,
            "2\n",
        );
    }

    #[test]
    fn test_tuple_errors() {
        test_error("1[0]", 0, 4, "", "expected a tuple\n");

        test_error("(1, 3, 5)[3]", 0, 6, "", "index too large\n");

        test_error("(1, 3, 5)[-1]", 0, 5, "", "index too small\n");
    }

    #[test]
    fn test_tuple_setting() {
        test_success("let x = (1, 2, 3) in x[1] = 5", 0, "5\n");
        test_success("let x = (1, 2), y = x[1] = 5 in x", 0, "(1, 5)\n");
        test_success("let x = (1, 2), y = x[1] = (3, 4) in x", 0, "(1, (3, 4))\n");
        test_success(
            "def f(x) { let y = (x[1] = (4, 5)) in x }f((1, 2, 3))",
            0,
            "(1, (4, 5), 3)\n",
        );
        // TODO: support loops in print
        //test_success("let x = (3, 4), y = x[1] = x in x", 0, "(3, (..))\n");
    }

    #[test]
    fn test_blocks() {
        test_success("begin\n5+10;\nend", 0, "15\n");
        test_success("begin\n5+10;13;\nend", 0, "13\n");
        test_success(
            "let t = (1, 2) in\nbegin\nt[0] = 3;\nt[1] = 4;t;end",
            0,
            "(3, 4)\n",
        );
        test_success(
            "def f(x, i) { if x == 0: i else: begin\n f(x - 1, i + 1);\nend } f(1000000, 0)",
            0,
            "1000000\n",
        );
    }

    #[test]
    fn test_tco() {
        test_success(
            "def f(n, i) { if n == 0: i else: f(n - 1, i + 1) }f(1000000, 0)",
            0,
            "1000000\n",
        );
    }

    #[test]
    fn test_even_odd() {
        test_success(
            "
def even(n) {
  if n == 0: true
  else:
    begin
      (1, 2, 3, n);
      odd(n - 1);
    end
}

def odd(n) {
  if n == 0: false
  else:
    begin
      ((n, 4, 5, 6, 7), true);
      even(n - 1);
    end
}

even(10731823)",
            0,
            "false\n",
        )
    }

    #[test]
    fn test_nested_tuples() {
        test_success(
            "(((((1, (2, 3, (4, (5, (6,7), 8),9),10),11)[1])[2])[1])[1])[1]",
            0,
            "7\n",
        )
    }

    #[test]
    fn test_tailcall1() {
        test_success(
            "
 def go(x) {
  if (x < 100000):
    go(x+1)
  else:
    x
  }
go(0)
",
            0,
            "100000\n",
        );
    }

    #[test]
    fn test_tailcall_overwrite1() {
        test_success(
            "
def f(w,x,y,z) { (w+x+y+z) }
def g(z,y,x,w) { f(w,x,y,z) }

g(1,2,3,4)
",
            0,
            "10\n",
        );
    }

    #[test]
    fn test_tailcall_overwrite2() {
        test_success(
            "
def f(x) {(x, x, x)}
def g(a, b, c) {f((a, b, c))}

g(1,2,3)
",
            0,
            "((1, 2, 3), (1, 2, 3), (1, 2, 3))\n",
        );
    }

    #[test]
    fn test_gc_simple() {
        test_gc("(1, 2, 3)", 4, 0, "(1, 2, 3)\n");
        test_gc(
            "
def f(x) { let y = (1, 2, 3) in 1 }
let x = (4, 5) in
begin
f(1);
print(x);
(6, 7);
end
        ",
            8,
            0,
            "(4, 5)\n(6, 7)\n",
        );
    }

    #[test]
    fn test_gc_full() {
        test(
            "((1, 2, 3), 4, 5)",
            7,
            0,
            1,
            "",
            Some("out of memory: Needed 4 words, but only 3 remain after collection"),
        );
    }

    #[test]
    fn test_gc_list_non_tc() {
        test_gc(
            "
def list(x) { (x, false) }

def listn(n, l) {
  if n == 0:
    l
  else:
    begin
      let x = (n * 17, l, 2) in x;
      (n, listn(n - 1, l));
    end
}

listn(20, list(0))
        ",
            90,
            0,
            "(20, (19, (18, (17, (16, (15, (14, (13, (12, (11, (10, (9, (8, (7, (6, \
                (5, (4, (3, (2, (1, (0, false)))))))))))))))))))))\n",
        );
    }

    #[test]
    fn test_gc_list_tc() {
        test_gc(
            "
def list(x) { (x, false) }

def listn(n, l) {
  if n == 0:
    l
  else:
    let x = (n * 5, 4, 10, l) in
      listn(n - 1, (n, l))
}

listn(20, list(0))
        ",
            90,
            0,
            "(1, (2, (3, (4, (5, (6, (7, (8, (9, (10, (11, (12, \
        (13, (14, (15, (16, (17, (18, (19, (20, (0, false)))))))))))))))))))))\n",
        );
    }

    #[test]
    fn test_gc_big() {
        test_gc(
            "
def f(x, i) {
  if x == 0: i
  else:
    begin
      (1, 2, 4, 5, 6, 7, 8, 9, 10);
      f(x - 1, i + 1);
    end
}
begin
(1, 2, 4);
f(50000, 0);
end",
            817,
            0,
            "50000\n",
        );
    }

    #[test]
    fn test_gc_cyclic_tuple() {
        test_gc(
            "
def f(a) {
  let x = (1, 2, 3) in begin
    x[0] = x;
    x[1] = (x, x);
  end
}

begin
f(1);
(1, 2, 3, 4, 5);
end",
            8,
            0,
            "(1, 2, 3, 4, 5)\n",
        );
    }

    #[test]
    fn test_lambda() {
        test_success("(\\x -> x + 5)(3)", 0, "8\n");

        test_success("let f = (\\x -> x * 2) in f(2)", 0, "4\n");

        test_success("let y = 5, f = (\\x -> x * y) in f(2)", 0, "10\n");

        test_success(
            "def g(f, x) { f(x * 2) }\nlet y = 5 in g((\\x -> x * y), 2)",
            0,
            "20\n",
        );
    }

    #[test]
    fn test_tuple_gc1() {
        test_gc(
            "let q = 3, h = (q, 2) in begin
        let y = 6, f = (y, 4) in 17;
        let a = 13, g = (a, 6) in 7;
        end",
            8,
            0,
            "7\n",
        );
    }

    #[test]
    fn test_lambda_gc1() {
        test_gc(
            "let q = 3, h = (\\x -> x + q) in begin
        let y = 6, f = (\\x -> x / y) in 17;
        let a = 13, g = (\\b -> a * b) in h(g(7));
        end",
            8,
            0,
            "94\n",
        );
    }

    #[test]
    fn test_tuple_lambda_gc() {
        test(
            "def g(x) { let y = 6, z = 9, f = (y, z, 5) in 17 }
let y = 13, h = (\\x -> x * y) in
begin
print(g(1));
h(2);
end
",
            7,
            0,
            1,
            "",
            Some("out of memory: Needed 4 words, but only 3 remain after collection"),
        );
    }

    #[test]
    fn test_lambda_gc2() {
        test(
            "def g(x) { let y = 6, z = 9, f = (\\x -> x + y + z) in 17 }
let y = 13, h = (\\x -> x * y) in
begin
print(g(1));
h(2);
end
",
            7,
            0,
            1,
            "",
            Some("out of memory: Needed 4 words, but only 3 remain after collection"),
        );
    }

    #[test]
    fn test_tuple_gc2() {
        test(
            "def g(x) { (7, 19, 5) }
let h = (1, 3), z = g(1) in h
",
            7,
            0,
            1,
            "",
            Some("out of memory: Needed 4 words, but only 3 remain after collection"),
        );
    }

    #[test]
    fn test_lambda_error() {
        test("(1)(1)", 10, 0, 7, "", Some("expected a lambda\n"));
        test("(1, 2)(0)", 10, 0, 7, "", Some("expected a lambda\n"));
        test("(\\x -> x)[1]", 10, 0, 4, "", Some("expected a tuple\n"));
    }

    #[test]
    fn test_call_in_lambda() {
        test_success(
            "def sq(x) { x * x } (\\x -> print(sq(x)) + 5)(7)",
            0,
            "49\n54\n",
        );
    }

    #[test]
    fn test_print_lambda() {
        test_success("(\\x -> x)", 0, "(λ)\n");
    }

    #[test]
    fn test_fn_arg() {
        test_success(
            "def call(f, x) { f(x) }\nlet f = (\\x -> x * 5) in call(f, 13)",
            0,
            "65\n",
        );
    }

    #[test]
    fn test_lambda_recursion() {
        let program = "
def callfn(p, x) { (p[0])(x) }

def sum(x) {
  let t = (0,0) in
  let f = (\\x -> (if x == 0: 0 else: x + callfn(t, (x - 1)))) in
  let u = t[0] = f in
  callfn(t,x)
}

sum(4)
";
        test_success(program, 0, "10\n")
    }

    #[test]
    fn test_map() {
        let program = "
def map(l, f) {
  if l == false: l
  else: (f(l[0]), map(l[1], f))
}

map((3, (2, (1, false))), (\\x -> x * 2))
";
        test_success(program, 0, "(6, (4, (2, false)))\n");
    }

    #[test]
    fn test_free_vars() {
        let program = parser::parse("(\\x -> x * (y + (let z = 3 in f(z))))");

        let mut env = vec![];
        let mut free = HashSet::new();
        let decls = vec![Decl {
            name: "f".to_string(),
            params: vec![],
            body: Number(1),
        }];
        Compiler::find_free_vars(&mut env, &decls, &program.init, &mut free);

        let mut expected = HashSet::new();
        expected.insert("y".to_string());
        assert_eq!(expected, free);
    }
}
