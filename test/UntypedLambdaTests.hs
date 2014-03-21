{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
module UntypedLambdaTests 
  ( allTests
  )
where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Set as Set

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Text.Parsec
import Text.Parsec.Error

import UntypedLambda.Parser
import UntypedLambda.Reduction
import UntypedLambda.Syntax

allTests :: [Test]
allTests = [basic, parsing, reduction]

basic :: Test
basic = testGroup "basic functions on lambda terms"
  [ testCase     "variables: var"      $ vars (Var "x")               @?= Set.singleton "x"
  , testCase     "variables: abs"      $ vars (Abs "x" (Var "y"))     @?= Set.fromList ["x", "y"]
  , testProperty "variables: app"      $ isUnionOfSubterms vars
  , testCase     "free variables: var" $ freeVars (Var "x")           @?= Set.singleton "x"
  , testCase     "free variables: abs" $ freeVars (Abs "x" (Var "x")) @?= Set.empty
  , testProperty "free variables: app" $ isUnionOfSubterms freeVars
  ]
  where isUnionOfSubterms f = \(t1, t2) -> f (App t1 t2) == f t1 `Set.union` f t2

parsing :: Test
parsing = testGroup "untyped lambda parsing" 
  [ testCase     "variable"     $ "x"          `assertParsesTo` Var "x"
  , testCase     "int constant" $ "42"         `assertParsesTo` Const 42
  , testCase     "abstraction"  $ "(\\x.y)"    `assertParsesTo` Abs "x" (Var "y")
  , testCase     "application"  $ "(x y)"      `assertParsesTo` App (Var "x") (Var "y")
  , testProperty "pretty print/parse round trip" 
                                $ mapSize (*50) $ \term -> prettyPrint term `parsesTo` term
  ]
  where
    parsesTo s t = parseString s == Right t
    assertParsesTo s t = parseString s @?= Right t

reduction :: Test
reduction = testGroup "untyped lambda term reduction"
  [ testProperty "head normal form is fixed point" $ \term -> headNormalForm term == (headNormalForm . headNormalForm) term
  , testProperty "head normal form does not contain beta-redex in head position" 
                                                   $ not . hasBetaRedexInHeadPosition . headNormalForm 
  , testCase     "beta reduction: name clash"
    $ assertBool "variables were incorrectly unified" $ betaReduce termWithVariableClash /= termWithIncorrectlyUnifiedVariables
  ]
  where
    hasBetaRedexInHeadPosition (App (Abs _ _) _) = True
    hasBetaRedexInHeadPosition _                 = False
    termWithVariableClash               = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
    termWithIncorrectlyUnifiedVariables = Abs "y" (App (Var "y") (Var "y"))

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

  shrink (Var ident)       = Var <$> shrink ident
  shrink (Const n)         = Const <$> shrink n
  shrink (BuiltIn _)       = []
  shrink (Abs _ term)      = [term]
  shrink (App term1 term2) = [term1, term2]

arbitraryId :: Gen String
arbitraryId = resize 2 $ listOf1 $ elements idChars

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b
