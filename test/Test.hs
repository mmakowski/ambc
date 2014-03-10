module Main 
where

import Test.Framework (Test, defaultMain)

import qualified UntypedLambdaTests

main :: IO () 
main = defaultMain tests

tests :: [Test]
tests = UntypedLambdaTests.allTests
