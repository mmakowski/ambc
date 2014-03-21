module UntypedLambda.Syntax 
  ( Term (..)
  , idChars
  , freeVars
  , prettyPrint
  )
where

import Data.Set (Set)
import qualified Data.Set as Set

idChars :: String
idChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_*/%^<>=:?!@"

data Term = Var String
          | Abs String Term
          | App Term Term
          | BuiltIn String
          | Const Integer
  deriving (Eq, Show)

prettyPrint :: Term -> String
prettyPrint (Var ident)       = ident
prettyPrint (BuiltIn ident)   = ident
prettyPrint (Const val)       = show val
prettyPrint (Abs ident term)  = "(\\" ++ ident ++ "." ++ prettyPrint term ++ ")"
prettyPrint (App term1 term2) = "(" ++ prettyPrint term1 ++ " " ++ prettyPrint term2 ++ ")"

freeVars :: Term -> Set String
freeVars = freeVars' []
  where
    freeVars' bound (Var ident)       = if elem ident bound then Set.empty else Set.singleton ident
    freeVars' bound (Abs ident term)  = freeVars' (ident:bound) term
    freeVars' bound (App term1 term2) = (freeVars' bound term1) `Set.union` (freeVars' bound term2)
    freeVars' _     _                 = Set.empty