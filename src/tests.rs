use untyped_lambda::{Abs, App, Var, parse};

mod untyped_lambda;

#[test]
fn parse_variable() {
    assert_eq!(~Var(~"x"), parse("x"));
}

#[test]
fn parse_abstraction() {
    assert_eq!(~Abs(~"x", ~Var(~"y")), parse("\\x.y"))
}

#[test]
fn parse_application() {
    assert_eq!(~App(~Var(~"x"), ~Var(~"y")), parse("x y"))
}
