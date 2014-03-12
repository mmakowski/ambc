{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
module UntypedLambdaTests 
  ( allTests
  )
where

import Control.Applicative
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
  [ testCase     "variable"    $ parseString "x"       @=? Right (Var "x")
  , testCase     "abstraction" $ parseString "(\\x.y)" @=? Right (Abs "x" (Var "y"))
  , testCase     "application" $ parseString "(x y)"   @=? Right (App (Var "x") (Var "y"))
  , testProperty "show/parse round trip" $ \term -> parseString (show term) == Right term
  ]

instance Arbitrary Term where
  arbitrary = sized arbitraryTerm
   where
     arbitraryTerm 0 = Var <$> arbitraryId
     arbitraryTerm n = oneof [ Var <$> arbitraryId
                             , Abs <$> arbitraryId <*> arbitraryTerm (n-1)
                             , App <$> arbitraryTerm (n-1) <*> arbitraryTerm (n-1)
                             ]

arbitraryId :: Gen String
arbitraryId = resize 10 $ listOf1 $ elements idChars

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b
