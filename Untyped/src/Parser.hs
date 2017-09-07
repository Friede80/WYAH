module Parser (
  parseExpr
) where

import           Syntax

import           Data.Char
import           Text.Parsec
import qualified Text.Parsec.Expr     as Ex
import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser $ haskellStyle
  { Tok.reservedNames = []
  , Tok.reservedOpNames = ["->","\\","+","*","-","="]
  , Tok.commentLine = "#"
  }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = fmap Var identifier

number :: Parser Expr
number = fmap (Lit . LInt . fromIntegral) natural

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

term :: Parser Expr
term =  parens expr
    <|> variable
    <|> number
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return $ foldl1 App es

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
