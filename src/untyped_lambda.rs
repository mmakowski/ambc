type VarName = ~str;

enum Term {
    Var(VarName),
    Abs(VarName, ~Term),
    App(~Term, ~Term)
}
