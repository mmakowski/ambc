type VarName = ~str;

#[deriving(Eq)]
pub enum Term {
    Var(VarName),
    Abs(VarName, ~Term),
    App(~Term, ~Term)
}

enum Token {
    TLambda,
    TName(~str),
    TDot,
    TLBr,
    TRBr
}

fn lex(input: &str) -> ~[Token] {
    let mut tokens = ~[];
    let mut pos = 0;
    while pos < input.len() {
        match input.char_at(pos) {
            '\\' => tokens.push(TLambda),
            '.'  => tokens.push(TDot),
            '('  => tokens.push(TLBr),
            ')'  => tokens.push(TRBr),
            ' '  => {}
            _    => { 
                    let (name, end_pos) = read_name(input, pos);
                    tokens.push(name);
                    pos = end_pos
                }
        }
        pos += 1
    }
    tokens
}

fn read_name(input: &str, start_pos: uint) -> (Token, uint) {
    let mut end_pos = start_pos;
    while end_pos < input.len() && is_id_char(input.char_at(end_pos)) { end_pos += 1 };
    if end_pos == start_pos { fail!(format!("expected identifier character but got {:?}", input.char_at(start_pos))) }
    (TName(input.slice_chars(start_pos, end_pos).to_owned()), end_pos - 1)
}

fn is_id_char(c: char) -> bool {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-+/*^".contains_char(c)
}

/*
grammar
=======
    <Term> ::= <Var> | <Abs> | <App>
    <Var>  ::= <name>
    <Abs>  ::= (\<name>.<Term>)
    <App>  ::= (<Term> <Term>)

*/

struct ParseResult<'a> { term: ~Term, remainder: &'a [Token] }

pub fn parse(input: &str) -> ~Term {
    let tokens = lex(input);
    let result = mk_term(tokens);
    match result.remainder {
        []    => result.term,
        other => fail!(format!("unexpected tokens: {:?}", other))
    }
}

fn mk_term<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    match tokens.head() {
        &TName(..) => ParseResult { term: ~Var(get_name(tokens.head())), remainder: tokens.tail() },
        &TLBr      => mk_abs_or_app(tokens.tail()),
        other      => fail!(format!("expected a name or '(' but got '{:?}'", *other))
    }
}

fn mk_abs_or_app<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    match tokens.head() {
        &TLambda => mk_abs(tokens),
        _        => mk_app(tokens)
    }
}

fn mk_abs<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    // TODO: validate lambda and dot
    let body = mk_term(tokens.slice_from(3));
    // TODO: validate remainder.head is RBr
    let remainder = body.remainder.tail();
    ParseResult { term: ~Abs(get_name(&tokens[1]), body.term), remainder: remainder }
}

fn mk_app<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    let first = mk_term(tokens);
    let second = mk_term(first.remainder);
    // TODO: validate remainder.head is RBr
    let remainder = second.remainder.tail();
    ParseResult { term: ~App(first.term, second.term), remainder: remainder }
}

fn get_name(token: &Token) -> ~str {
    match token {
        &TName(ref n) => n.to_owned(),
        other         => fail!(format!("expected a name but got {:?}", *other))
    }
}

#[cfg(test)]
mod test {
    use super::{Var, Abs, App, parse};

    #[test]
    fn parse_variable_single_letter() {
        assert_eq!(parse("x"), 
                   ~Var(~"x"))
    }

    #[test]
    fn parse_variable_alnum() {
        assert_eq!(parse("some_Alnum-131"), 
                   ~Var(~"some_Alnum-131"))
    }

    #[test]
    fn parse_abstraction() {
        assert_eq!(parse("(\\x.y)"),
                   ~Abs(~"x", ~Var(~"y")))
    }

    #[test]
    fn parse_application_simple() {
        assert_eq!(parse("(x y)"),
                   ~App(~Var(~"x"), ~Var(~"y")))
    }

    #[test]
    fn parse_nested_applications() {
        assert_eq!(parse("((x y)(z u))"),
                   ~App(~App(~Var(~"x"), ~Var(~"y")), ~App(~Var(~"z"), ~Var(~"u"))))
    }

    #[test]
    fn parse_complex_expression() {
        assert_eq!(parse("(\\x_1.(free (\\snd.(snd x_1))))"),
                   ~Abs(~"x_1", ~App(~Var(~"free"), ~Abs(~"snd", ~App(~Var(~"snd"), ~Var(~"x_1"))))))
    }
}
