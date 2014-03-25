{-| A simple functional language inspired by Core language from SPJ's tutorial.
The syntax is a bit more complex than that of untyped lambda calculus, but compilation to
real machine code is more straightforward.
-}
module Iota.Syntax
  ( Module (..) 
  , FunDef (..)
  , Expr (..)
  , Name
  )
where

data Module = Defs [FunDef] deriving (Eq, Show)

data FunDef = FunDef { name   :: Name
                     , params :: [Name]
                     , body   :: Expr
                     }
            deriving (Eq, Show)

data Expr = App [Expr]
          | Var Name
          | Const Integer
          | Fun Name
          deriving (Eq, Show)

type Name = String
