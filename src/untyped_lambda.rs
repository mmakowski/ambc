type VarName = ~str;

#[deriving(Eq)]
pub enum Term {
    Var(VarName),
    Abs(VarName, ~Term),
    App(~Term, ~Term)
}

pub fn parse(input: &str) -> ~Term {
    return if (input.starts_with("\\")) { 
        ~Abs(input.slice_chars(1, 2).to_owned(), parse(input.slice_from(3)))
    } else {
        ~Var(input.to_owned())
    };
}
