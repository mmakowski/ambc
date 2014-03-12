{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
module UntypedLambdaTests 
  ( allTests
  )
where

import Control.Monad (liftM, liftM2)
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
allTests = [parsing, parsingQC]

parsing :: Test
parsing = testGroup "untyped lambda parsing" 
  [ testCase "variable"    (parseString "x"       @=? Right (Var "x"))
  , testCase "abstraction" (parseString "(\\x.y)" @=? Right (Abs "x" (Var "y")))
  , testCase "application" (parseString "(x y)"   @=? Right (App (Var "x") (Var "y")))
  ]

arbitraryId :: Gen String
arbitraryId = resize 10 $ listOf1 $ elements idChars

instance Arbitrary Term where
  arbitrary = oneof [ liftM Var arbitraryId
                    , liftM2 Abs arbitraryId arbitrary
                    , liftM2 App arbitrary arbitrary
                    ]

parsingQC :: Test
parsingQC = testGroup "untyped lambda parsing QC" 
  [ testProperty "show/parse round trip" $ \term -> parseString (show term) == Right term
  ]

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b
