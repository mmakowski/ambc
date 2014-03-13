module UntypedLambda.Syntax 
  ( Term (..)
  , idChars
  , prettyPrint
  )
where

idChars :: String
idChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_*/%^<>=:?!@"

data Term = Var String
          | BuiltIn String
          | Const Integer
          | Abs String Term
          | App Term Term
  deriving (Eq, Show)

prettyPrint :: Term -> String
prettyPrint (Var ident)       = ident
prettyPrint (BuiltIn ident)   = ident
prettyPrint (Const val)       = show val
prettyPrint (Abs ident term)  = "(\\" ++ ident ++ "." ++ prettyPrint term ++ ")"
prettyPrint (App term1 term2) = "(" ++ prettyPrint term1 ++ " " ++ prettyPrint term2 ++ ")"
