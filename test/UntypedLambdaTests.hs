{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
module UntypedLambdaTests 
  ( allTests
  )
where

import Control.Applicative ((<$>), (<*>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Text.Parsec
import Text.Parsec.Error

import UntypedLambda.Parser
import UntypedLambda.Syntax

allTests :: [Test]
allTests = [parsing]

parsing :: Test
parsing = testGroup "untyped lambda parsing" 
  [ testCase     "variable"     $ "x"          `assertParsesTo` Var "x"
  , testCase     "int constant" $ "42"         `assertParsesTo` Const 42
  , testCase     "abstraction"  $ "(\\x.y)"    `assertParsesTo` Abs "x" (Var "y")
  , testCase     "application"  $ "(x y)"      `assertParsesTo` App (Var "x") (Var "y")
  , testProperty "pretty print/parse round trip" $ mapSize (*50) $ \term -> prettyPrint term `parsesTo` term
  ]

instance Arbitrary Term where
  arbitrary = sized arbitraryTerm
   where
     arbitraryTerm 0 = atom
     arbitraryTerm n = frequency [(1, atom), (10, compound n)]
     atom = oneof [ Var <$> arbitraryId
                  , Const <$> arbitrary
                  ]
     compound n = oneof [ Abs <$> arbitraryId <*> arbitraryTerm (n-1)
                        , App <$> arbitraryTerm (n `div` 2) <*> arbitraryTerm (n `div` 2)
                        ]

parsesTo :: String -> Term -> Bool
parsesTo s t = parseString s == Right t

assertParsesTo :: String -> Term -> Assertion
assertParsesTo s t = parseString s @?= Right t

arbitraryId :: Gen String
arbitraryId = resize 10 $ listOf1 $ elements idChars

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b
