module UntypedLambda.Parser
  ( contents
  , pTerm
  )
where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

import UntypedLambda.Syntax

pTerm :: Parser Term
pTerm =  try pApp
     <|> try pAbs
     <|> pVar

pApp :: Parser Term
pApp = do
  verbatim "("
  term1 <- pTerm
  verbatim " "
  term2 <- pTerm
  verbatim ")"
  return $ App term1 term2

pAbs :: Parser Term
pAbs = do
  verbatim "(\\"
  ident <- identifier
  verbatim "."
  term <- pTerm
  verbatim ")"
  return $ Abs ident term

pVar :: Parser Term
pVar = do
  ident <- identifier
  return $ Var ident

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

identifier :: Parser String
identifier = T.identifier lexer

verbatim :: String => Parser ()
verbatim s = do 
  _ <- string s
  return ()
