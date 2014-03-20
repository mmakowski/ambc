module Main
where

import Control.Monad.Trans
import System.Console.Haskeline

import UntypedLambda.Parser
import UntypedLambda.Reduction
import UntypedLambda.Syntax

process :: String -> IO ()
process line = do
  let res = parseString line
  case res of
    Left err -> print err
    Right ex -> putStrLn (prettyPrint $ headNormalForm ex)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "u\\> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
