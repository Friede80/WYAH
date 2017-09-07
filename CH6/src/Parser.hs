module Parser (
  parseExpr
) where

import           Syntax
import           Type

import           Data.Char
import           Text.Parsec
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
  args <- many1 (parens lambdaArg)
  reservedOp "."
  body <- expr
  return $ foldr (uncurry Lam) body args

lambdaArg :: Parser (Name, Type)
lambdaArg = do
  arg <- identifier
  reservedOp ":"
  t <- argType
  return (arg, t)

argType :: Parser Type
argType = (reservedOp "Int" >> return TInt)
      <|> (reservedOp "Bool" >> return TBool)
      <|> parens functionType

functionType :: Parser Type
functionType = do
  lhs <- argType
  reservedOp "->"
  rhs <- argType
  return $ TArr lhs rhs

term :: Parser Expr
term =  parens expr
    <|> bool
    <|> variable
    <|> number
    <|> lambda
    <|> primOp

primOp :: Parser Expr
primOp = do
  op <- (reservedOp "+" >> return Add)  <|> (reservedOp "*" >> return Mul)
  t1 <- term
  t2 <- term
  return $ Prim op t1 t2

bool :: Parser Expr
bool =  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

expr :: Parser Expr
expr = do
  es <- many1 term
  return $ foldl1 App es

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
