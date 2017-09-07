module Type where

data Type
  = TBool
  | TNat
  | TArr Type Type
  deriving (Eq)
