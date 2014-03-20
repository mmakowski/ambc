module UntypedLambda.Reduction 
  ( headNormalForm )
where 

import UntypedLambda.Syntax

headNormalForm :: Term -> Term
headNormalForm = until (\t -> betaReduce t == t) betaReduce

betaReduce :: Term -> Term
betaReduce (App (Abs v t1) t2) = subst v t2 t1
betaReduce t = t

subst :: String -> Term -> Term -> Term
subst v t1 t2@(Var ident)    = if ident == v then t1 else t2
subst v t1    (Abs ident t2) = Abs ident (subst v t1 t2)
subst v t1    (App t2 t3)    = App (subst v t1 t2) (subst v t1 t3)
subst _ _  t2                = t2
