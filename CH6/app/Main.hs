module Main where

import           Check
import           Eval
import           Parser

import System.IO

showLeft :: (Show a) => Either a b -> Either String b
showLeft (Left errMsg) = Left $ show errMsg
showLeft (Right x)     = return x

flattenEither :: Either a a -> a
flattenEither (Left x)  = x
flattenEither (Right x) = x

process :: String -> String
process [] = []
process line = flattenEither $ do
  expr <- showLeft $ parseExpr line
  _ <- showLeft $ checkExpr expr
  return $ show (runEval expr)

main :: IO ()
main = hSetBuffering stdout NoBuffering >> loop
 where
  loop = do
    putStr "Typed> "
    input <- getLine
    putStrLn (process input) >> loop
