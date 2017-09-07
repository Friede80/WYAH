module Syntax where

import Type

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Type Expr
  | Lit Lit

data Lit
  = LInt Int
  | LBool Bool
