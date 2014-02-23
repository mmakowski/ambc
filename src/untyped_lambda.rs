type VarName = ~str;

#[deriving(Eq)]
pub enum Term {
    Var(VarName),
    Abs(VarName, ~Term),
    App(~Term, ~Term)
}

pub fn parse(input: ~str) -> Term {
    return Var(input);
}
