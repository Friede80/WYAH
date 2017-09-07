module Check where

import Type
import Syntax
import Pretty

import Control.Monad.Except

data TypeError = TypeMismatch Type Type

instance Show TypeError where
  show (TypeMismatch a b) = "TypeMismatch: Got " ++ pprint a ++ " but expected " ++ pprint b

type Check a = Except TypeError a

check :: Expr -> Either TypeError Type
check = runExcept . typeOf

typeOf :: Expr -> Check Type
typeOf (Succ a) = do
  ta <- typeOf a
  case ta of
    TNat -> return TNat
    _ -> throwError $ TypeMismatch ta TNat

typeOf (Pred a) =do
  ta <- typeOf a
  case ta of
    TNat -> return TNat
    _ -> throwError $ TypeMismatch ta TNat

typeOf (IsZero a) = do
  ta <- typeOf a
  case ta of
    TBool -> return TBool
    t -> throwError $ TypeMismatch t TBool

typeOf (If p t e) = do
  tp <- typeOf p
  tt <- typeOf t
  te <- typeOf e
  if tp /= TBool
    then throwError $ TypeMismatch tp TBool
    else if tt /= te
      then throwError $ TypeMismatch tt te
      else return tt
      
typeOf Tr = return TBool
typeOf Fl = return TBool
typeOf Zero = return TNat
