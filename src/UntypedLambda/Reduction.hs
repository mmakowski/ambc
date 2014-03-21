module UntypedLambda.Reduction 
  ( betaReduce
  , headNormalForm 
  )
where 

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map


import UntypedLambda.Syntax

headNormalForm :: Term -> Term
headNormalForm = until (\t -> betaReduce t == t) betaReduce

betaReduce :: Term -> Term
betaReduce (App (Abs v t1) t2) = subst v t2 t1
betaReduce t = t

subst :: String -> Term -> Term -> Term
subst ident term1 term2 = let term1' = unclash term1 (vars term2)
                              term2' = unclash term2 (vars term1')
                          in subst' ident term1' term2'
  where
    subst' v t1 t2@(Var i)    = if i == v then t1 else t2
    subst' v t1    (Abs i t2) = Abs i (subst' v t1 t2)
    subst' v t1    (App t2 t3)    = App (subst' v t1 t2) (subst' v t1 t3)
    subst' _ _  t2                = t2

unclash :: Term -> Set String -> Term
unclash term cl = unclash' term cl Map.empty
  where 
    unclash' (Var ident) _ renames = Var $ Map.findWithDefault ident ident renames
    unclash' orig@(Abs ident t) clashing renames = if ident `Set.member` clashing then 
                                                     let ident'    = freshVar ident clashing
                                                         clashing' = Set.insert ident' clashing
                                                         renames'  = Map.insert ident ident' renames
                                                     in (Abs ident' (unclash' t clashing' renames'))
                                                   else orig
    unclash' (App t1 t2) clashing renames = App (unclash' t1 clashing renames) (unclash' t2 clashing renames)
    unclash' t _ _ = t

freshVar :: String -> Set String -> String
freshVar ident used = let ident' = ident ++ "'" 
                      in if ident' `Set.member` used then freshVar ident' used else ident'
