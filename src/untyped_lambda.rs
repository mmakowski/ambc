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
    TSpace
}

fn lex(input: &str) -> ~[Token] {
    let mut tokens = ~[];
    let mut pos = 0;
    while pos < input.len() {
        match input.char_at(pos) {
            '\\' => tokens.push(TLambda),
            '.'  => tokens.push(TDot),
            ' '  => tokens.push(read_space(input, &pos)),
            _    => tokens.push(read_name(input, &pos))
        }
        pos += 1
    }
    tokens
}

fn read_space(input: &str, pos: &uint) -> Token {
    // TODO: read until char that is not space or end of input
    TSpace
}

fn read_name(input: &str, pos: &uint) -> Token {
    // TODO: read until char that is not part of name
    TName(input.slice_chars(*pos, *pos + 1).to_owned())
}

pub fn parse(input: &str) -> ~Term {
    mk_term(lex(input))
}

fn mk_term(tokens: &[Token]) -> ~Term {
    match tokens.head() {
        &TLambda  => mk_abs(tokens),
        &TName(_) => mk_var_or_app(tokens),
        other     => fail!(format!("expected a lambda or a name but got {:?}", *other))
    }
}

fn mk_abs(tokens: &[Token]) -> ~Term {
   ~Abs(get_name(&tokens[1]), mk_term(tokens.slice_from(3)))
}

fn mk_var_or_app(tokens: &[Token]) -> ~Term {
    let first_term = ~Var(get_name(tokens.head()));
    if tokens.len() == 1 { 
        first_term 
    } else {
        match &tokens[1] {
            &TSpace => ~App(first_term, mk_term(tokens.slice_from(2))),
            other   => fail!(format!("expected a whitespace but got {:?}", *other))
        }
    }
}

fn get_name(token: &Token) -> ~str {
    match token {
        &TName(ref n) => n.to_owned(),
        other         => fail!(format!("expected a name but got {:?}", *other))
    }
}
