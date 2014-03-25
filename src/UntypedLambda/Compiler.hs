module UntypedLambda.Compiler
  ( toIota )
where

import Prelude hiding (id)
import UntypedLambda.Syntax
import qualified Iota.Syntax as I

toIota :: Term -> I.Module
toIota = I.Defs . topLevel

topLevel :: Term -> [I.FunDef]
topLevel (Const n)   = [I.FunDef "main" [] (I.Const n)]
topLevel t@(App _ _) = defs ("main":names) t
topLevel t           = error $ "no valid translation of " ++ (show t) ++ "into a list of function definitions"

defs :: [I.Name] -> Term -> [I.FunDef]
defs (id:ids) (App t1 t2) = defs ids t2 ++ liftedLambdas ++ [I.FunDef id (params t1') (body t1')]
  where (t1', liftedLambdas) = lambdaLift t1
defs _ _ = error "TODO"

lambdaLift :: Term -> (Term, [I.FunDef])
lambdaLift _ = (Var "x", [])

params :: Term -> [I.Name]
params _ = []

body :: Term -> I.Expr
body _ = I.Const 42

names :: [I.Name]
names = "a" : (map (++"'") names)
  