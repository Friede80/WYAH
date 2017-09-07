module Main where

import NanoParsec
import Control.Monad
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    putStr "> "
    a <- getLine
    print . eval $ run a
