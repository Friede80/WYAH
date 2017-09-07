module Main where

import Eval
import Parser
import Pretty
import Check

import Control.Monad.Trans
import System.Console.Haskeline
import Data.Maybe
import Data.Either

showLeft :: (Show a) => Either a b -> Either String b
showLeft (Left errMsg) = Left $ show errMsg
showLeft (Right x) = return x

flattenEither :: Either a a -> a
flattenEither (Left x)  = x
flattenEither (Right x) = x

process :: String -> String
process line = flattenEither $ do
  expr <- showLeft $ parseExpr line
  texpr <- showLeft $ check expr
  return $ pprint (fromJust (eval expr)) ++ " : " ++ pprint texpr

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "Calc> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> outputStrLn (process input) >> loop
