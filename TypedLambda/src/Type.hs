module Type where

data Type
  = TBool
  | TInt
  | TArr Type Type
  deriving (Eq)
