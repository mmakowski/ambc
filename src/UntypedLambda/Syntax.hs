module UntypedLambda.Syntax 
  ( idChars
  , Term (..)
  )
where

idChars :: [Char]
idChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_+-*/%^<>=:?!@"

data Term = Var String
          | Abs String Term
          | App Term Term
  deriving (Eq)

instance Show Term where
  show (Var ident)                     = ident
  show (Abs ident term)                = "(\\" ++ ident ++ "." ++ (show term) ++ ")"
  show (App (Var ident1) (Var ident2)) = "(" ++ ident1 ++ " " ++ ident2 ++ ")"
  show (App term1 term2)               = "(" ++ (show term1) ++ (show term2) ++ ")"
