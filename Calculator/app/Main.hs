module Main where

import Eval
import Parser
import Pretty

import Control.Monad.Trans
import System.Console.Haskeline
import Data.Maybe

process :: String -> String
process line =
  case parseExpr line of
    Left err -> show err
    Right ex -> fromMaybe "Cannot evaluate" (ppexpr <$> eval ex)

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "Repl> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> outputStrLn (process input) >> loop
