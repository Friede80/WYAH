module Main where

import           Check
import           Eval
import           Parser
import           Pretty
import           Syntax

import           Control.Monad.Except
import           Control.Monad.Trans
import           Data.Maybe
import           System.Console.Haskeline

showLeft :: (Show a) => Either a b -> Either String b
showLeft (Left errMsg) = Left $ show errMsg
showLeft (Right x)     = return x

flattenEither :: Either a a -> a
flattenEither (Left x)  = x
flattenEither (Right x) = x

process :: String -> String
process line = flattenEither $ do
  expr <- showLeft $ parseExpr line
  _ <- showLeft $ checkExpr expr
  return $ show (runEval expr)

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "Typed> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> outputStrLn (process input) >> loop
