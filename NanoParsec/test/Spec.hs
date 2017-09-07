import NanoParsec
import Data.Char
import Test.QuickCheck
import Criterion.Main

prop_parseNum :: String -> Bool
prop_parseNum s@(_c:_cs) | all isDigit s = parse natural s == [(read s, "")]
prop_parseNum _ = True

test1 :: String
test1 = "1+2+3+4+5+6"

test2 :: String
test2 = "1*2*3*4*5*6"

test3 :: String
test3 = "1-2+3*4-5+6"

main :: IO ()
main = do
    quickCheck prop_parseNum
    defaultMain [ bgroup "Parse Expression"
                    [ bench test1  $ whnf (eval . run) test1
                    , bench test2  $ whnf (eval . run) test2
                    , bench test3  $ whnf (eval . run) test3
                    ]
                ]
