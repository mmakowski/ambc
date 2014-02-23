use untyped_lambda::{Var, parse};

mod untyped_lambda;

#[test]
fn parse_variable() {
    assert_eq!(Var(~"x"), parse(~"x"));
}
