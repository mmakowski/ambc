module Main 
where

import Data.Monoid (mempty)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework (Test, defaultMainWithOpts)

import qualified UntypedLambdaTests

main :: IO () 
main = do
  let emptyTestOpts = mempty :: TestOptions
  let testOpts = emptyTestOpts { topt_maximum_generated_tests = Just 500 }
  let emptyRunnerOpts = mempty :: RunnerOptions
  let runnerOpts = emptyRunnerOpts { ropt_test_options = Just testOpts }
  defaultMainWithOpts tests runnerOpts

tests :: [Test]
tests = UntypedLambdaTests.allTests
