{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
module Main 
where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

import Text.Parsec
import Text.Parsec.Error

import UntypedLambda.Parser
import UntypedLambda.Syntax

main :: IO () 
main = defaultMain tests

tests :: [Test]
tests = [untypedLambdaParsing]

untypedLambdaParsing :: Test
untypedLambdaParsing = testGroup "untyped lambda parsing" 
  [ testCase "variable"    (parseUL "x"       @=? (Right (Var "x")))
  , testCase "abstraction" (parseUL "(\\x.y)" @=? (Right (Abs "x" (Var "y"))))
  , testCase "application" (parseUL "(x y)"   @=? (Right (App (Var "x") (Var "y"))))
  ]

parseUL :: String -> Either ParseError Term
parseUL = parse (contents pTerm) "<stdin>"

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b
