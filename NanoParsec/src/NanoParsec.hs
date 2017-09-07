module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)] }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (a, b) <- parse p s
    return (f a, b)

instance Applicative Parser where
  pure a = Parser $ \s -> [(a,s)]
  p1 <*> p2 = Parser $ \s -> do
    (f, s1) <- parse p1 s
    (a, s2) <- parse p2 s1
    return (f a, s2)

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s -> do
    (a, s') <- parse p s
    parse (f a) s'

instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus p1 p2 = Parser $ \s -> parse p1 s ++ parse p2 s

instance Alternative Parser where
  empty = mzero
  p1 <|> p2 = Parser $ \s ->
    case parse p1 s of
      []  -> parse p2 s
      res -> res

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res,[])] -> res
    [(_, rs)]  -> error "Parser did not consume entire stream"
    _          -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = do
  c <- item
  if cond c
    then return c
    else empty

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = chainl1 p op <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest a = ( do
                f <- op
                b <- p
                rest (f a b) )
          <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some digit

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

parens :: Parser a -> Parser a
parens p = do
  reserved "("
  n <- p
  reserved ")"
  return n

-- Haskell Part

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Sub a b) = eval a - eval b
eval (Lit n)   = n

int :: Parser Expr
int = do
  n <- number
  return $ Lit n

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp op f = reserved op >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" Add <|> infixOp "-" Sub

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

factor :: Parser Expr
factor = int <|> parens expr

term :: Parser Expr
term = chainl1 factor mulop

expr :: Parser Expr
expr = chainl1 term addop

run :: String -> Expr
run = runParser expr
