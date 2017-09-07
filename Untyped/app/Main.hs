module Main where

import Eval
import Parser
import Pretty
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline
import Data.Maybe

showStep :: (Int, Expr) -> String
showStep (d, e) = replicate d ' ' ++ "=>" ++ ppexpr e

process :: String -> String
process line =
  case parseExpr line of
    Left err -> show err
    Right ex ->
      let (out, steps) = runEval ex
      in unlines $ fmap showStep steps ++ [show out]

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "Untyped> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> outputStrLn (process input) >> loop
