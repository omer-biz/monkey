use std::fmt::Display;

use crate::{
    ast::{ExpressionStatement, Expressions, Node, Statements},
    lexer::Lexer,
};

use super::Parser;

#[test]
pub fn test_let_statements() {
    use super::Parser;
    use crate::lexer::Lexer;

    let input = r#"
        let x = 5;
        let y = 10;
        let foobar = barfoo;
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.statements.len() != 3 {
        panic!("program.statments does not contain 3 statements")
    }

    let exprected_ident = ["x", "y", "foobar"];

    for (i, ident) in exprected_ident.iter().enumerate() {
        let stmt = program
            .statements
            .get(i)
            .expect("statment not found at index");

        test_let_statement(stmt, ident);
    }
}

#[allow(dead_code)]
fn check_parser_errors(parser: &super::Parser) {
    if parser.errors.is_empty() {
        return;
    }

    for e in parser.errors.iter() {
        println!("{}", e);
    }

    panic!("parser error");
}

#[allow(dead_code)]
pub fn test_let_statement(stmt: &Statements, ident: &str) {
    if stmt.token_literal() != "let" {
        panic!("stmt.token_literal not 'let'. got={}", stmt.token_literal())
    }

    let Statements::LetStmt(let_stmt) = stmt else { panic!("Statment not `LetStatment`") };

    if let_stmt.name.value != ident {
        panic!(
            "let_stmt.name.value not {} got {}",
            ident, let_stmt.name.value
        );
    }

    if let_stmt.name.token_literal() != ident {
        panic!(
            "let_stmt.name.token_literal() not {} got {}",
            ident,
            let_stmt.name.token_literal()
        )
    }
}

#[test]
pub fn test_return_statements() {
    use super::Parser;
    use crate::lexer::Lexer;

    let input = r#"
        return 5;
        return x;
        return x + 1;
        return 10 + 1;
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.statements.len() != 4 {
        panic!("program.statments does not contain 4 statements")
    }

    for stmt in program.statements.iter() {
        let Statements::RetStmt(return_statement) = stmt else { panic!("not return statement got {:?}", stmt) };
        if return_statement.token_literal() != "return" {
            panic!(
                "return_statement.token_literal() not `return` got {:?}",
                return_statement.token_literal()
            )
        }
    }
}

#[test]
pub fn test_as_string() {
    use crate::{
        ast::{Expressions, Identifier, LetStatement, Program, Statements},
        token::{Token, TokenType},
    };

    let program = Program {
        statements: vec![Statements::from(LetStatement {
            token: Token {
                ttype: TokenType::LET,
                literal: "let".to_string(),
            },
            name: Identifier {
                token: Token {
                    ttype: TokenType::IDENT,
                    literal: "my_var".to_string(),
                },
                value: "my_var".to_string(),
            },
            value: Expressions::from(Identifier {
                token: Token {
                    ttype: TokenType::IDENT,
                    literal: "another_var".to_string(),
                },
                value: "another_var".to_string(),
            }),
        })],
    };

    if program.to_string() != "let my_var = another_var;" {
        panic!("program.to_string() wrong. got={:?}", program.to_string());
    }
}

#[test]
pub fn test_identifer_expression() {
    use super::Parser;
    use crate::{
        ast::{Expressions, Statements},
        lexer::Lexer,
    };

    let input = r#"
        foobar;
        barfoo;
        deadbeef;
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.statements.len() != 3 {
        panic!()
    }

    let expected_ident = ["foobar", "barfoo", "deadbeef"];

    for (i, expression) in program.statements.iter().enumerate() {
        let Statements::ExpStmt(exp_stmt) = expression else { panic!() };

        let Expressions::Ident(ident) = &exp_stmt.expression else { panic!() };

        if ident.value != expected_ident[i] {
            panic!();
        }

        if ident.token_literal() != expected_ident[i] {
            panic!();
        }
    }
}

#[test]
pub fn test_integer_literal_expression() {
    use super::Parser;
    use crate::{
        ast::{Expressions, Statements},
        lexer::Lexer,
    };

    let input = r#"
        10;
        11;
        19;
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.statements.len() != 3 {
        panic!()
    }

    let expected_ident = [10f64, 11f64, 19f64];

    for (i, expression) in program.statements.iter().enumerate() {
        let Statements::ExpStmt(exp_stmt) = expression else { panic!() };

        let Expressions::Integ(literal) = &exp_stmt.expression else { panic!() };

        if literal.value != expected_ident[i] {
            panic!();
        }

        if literal.token_literal() != expected_ident[i].to_string() {
            panic!();
        }
    }
}

#[test]
pub fn test_parsing_prefix_expressions() -> Result<(), ()> {
    use super::Parser;
    use crate::{
        ast::{Expressions, Statements},
        lexer::Lexer,
    };

    struct PrefixTest<'a> {
        pub input: &'a str,
        pub operator: char,
        pub integer_value: f64,
    }

    let prefix_tests = vec![
        PrefixTest {
            input: "!5.1;",
            operator: '!',
            integer_value: 5.1,
        },
        PrefixTest {
            input: "-15;",
            operator: '-',
            integer_value: 15.0,
        },
    ];

    for tt in &prefix_tests {
        let lexer = Lexer::new(tt.input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!()
        }

        let Statements::ExpStmt(stmt) = program.statements.get(0).ok_or(())? else { panic!() };
        let Expressions::PrExp(exp) = &stmt.expression else { panic!() };

        if exp.operator != tt.operator {
            panic!()
        }

        test_integer_literal(&exp.right, tt.integer_value);
    }

    Ok(())
}

#[allow(dead_code)]
fn test_integer_literal(right: &Expressions, integer_value: f64) {
    let Expressions::Integ(integ) = right else { panic!() };
    if integ.value != integer_value {
        panic!("expected {} found {}", integer_value, integ.value);
    }

    if integ.token_literal() != integer_value.to_string() {
        panic!()
    }
}

#[test]
fn test_parsing_infix_expressions() {
    use super::Parser;
    use crate::{
        ast::{Expressions, Statements},
        lexer::Lexer,
    };

    let infix_tests: [(&str, f64, &str, f64); 8] = [
        ("5 + 5;", 5.0, "+", 5.0),
        ("5 - 5;", 5.0, "-", 5.0),
        ("5 * 5;", 5.0, "*", 5.0),
        ("5 / 5;", 5.0, "/", 5.0),
        ("5 > 5;", 5.0, ">", 5.0),
        ("5 < 5;", 5.0, "<", 5.0),
        ("5 == 5;", 5.0, "==", 5.0),
        ("5.1 != 5.1;", 5.1, "!=", 5.1),
    ];

    for (input, left_value, operator, right_value) in infix_tests {
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement"
        );

        let Statements::ExpStmt(stmt) = &program.statements[0] else { panic!("program.statements[0] is not ast.ExpressionStatement") };

        let Expressions::InExp(exp) = &stmt.expression else { panic!("exp is not ast.InfixExpression") };

        test_integer_literal(&exp.left, left_value);
        assert_eq!(exp.operator, operator, "exp.Operator is not '{}'", operator);
        test_integer_literal(&exp.right, right_value);
    }
}

#[test]
fn test_operator_precedence_parsing() {
    use super::Parser;
    use crate::lexer::Lexer;

    let tests = [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3.1 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3.1 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
    ];

    for (input, expected) in tests {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        let actual = program.to_string();
        assert_eq!(actual, expected.to_string());
    }
}

#[allow(dead_code)]
fn test_identifer(exp: &Expressions, value: &str) {
    let Expressions::Ident( ident ) = exp else { panic!() };

    if ident.value != value {
        panic!()
    }

    if ident.token_literal() != value {
        panic!()
    }
}

#[test]
pub fn test_boolean_expression() {
    use super::Parser;
    use crate::{
        ast::{Expressions, Statements},
        lexer::Lexer,
    };

    let input = r#"
        true;
        false;
        false;
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.statements.len() != 3 {
        panic!()
    }

    let expected_ident = [true, false, false];

    for (i, expression) in program.statements.iter().enumerate() {
        let Statements::ExpStmt(exp_stmt) = expression else { panic!() };

        let Expressions::Boole(literal) = &exp_stmt.expression else { panic!() };

        if literal.value != expected_ident[i] {
            panic!();
        }

        if literal.token_literal() != expected_ident[i].to_string() {
            panic!();
        }
    }
}

#[test]
pub fn test_if_expression() {
    let input = "if (x < y) { x }";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!()
    }

    let Statements::ExpStmt(stmt) = &program.statements[0] else { panic!() };
    let Expressions::IFExp(exp) = &stmt.expression else { panic!(); };
    test_infix_expressions(&exp.condition, "x", "<", "y");

    if exp.consequence.statements.len() != 1 {
        panic!();
    }

    let Statements::ExpStmt(consequence) = &exp.consequence.statements[0] else { panic!() };

    if consequence.as_string() != "x" {
        panic!();
    }

    if exp.alternative.is_some() {
        panic!();
    }

    let input = "if (x < y) { x } else { y }";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    let Statements::ExpStmt(stmt) = &program.statements[0] else { panic!() };
    let Expressions::IFExp(exp) = &stmt.expression else { panic!(); };

    if let Some(alt) = &exp.alternative {
        let Statements::ExpStmt(consequence) = &alt.statements[0] else { panic!() };

        if consequence.as_string() != "y" {
            panic!();
        }
    }
}

fn test_infix_expressions(
    condition: &Expressions,
    left: impl Display,
    operator: impl Display,
    right: impl Display,
) {
    let Expressions::InExp(op_exp) = condition else { panic!() };

    assert!(op_exp.operator == operator.to_string());
    assert!(op_exp.left.as_string() == left.to_string());
    assert!(op_exp.right.as_string() == right.to_string());
    assert!(op_exp.operator == operator.to_string());
}

#[test]
fn test_function_literal_parsing() {
    let input = r#"fn(x, y) { x + y; }"#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    assert!(program.statements.len() == 1);

    let Statements::ExpStmt(exp) = &program.statements[0] else { panic!() };
    let Expressions::FnLit(function) = &exp.expression else { panic!() };

    assert!(
        function.parameters.len() == 2,
        "unexpected number function parameters"
    );

    let Statements::ExpStmt(body_stmt) = &function.body.statements[0] else { panic!() };
    test_infix_expressions(&body_stmt.expression, "x", "+", "y");
}

#[test]
fn test_function_parameters_parsing() {
    let test = [
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x, y, z) {};", vec!["x", "y", "z"]),
    ];

    for (input, expected) in test {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        let Statements::ExpStmt(stmt) = &program.statements[0] else { panic!() };
        let Expressions::FnLit(function) = &stmt.expression else { panic!() };

        assert!(
            function.parameters.len() == expected.len(),
            "unexpected number function parameters"
        );

        for (i, ident) in expected.iter().enumerate() {
            let p = &function.parameters[i];
            assert!(ident.eq(&p.value), "unexpected identifier");
        }
    }
}
