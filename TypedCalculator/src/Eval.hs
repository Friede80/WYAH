module Eval where

import Syntax

import Data.Maybe
import Data.Functor

isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr          = True
isVal Fl          = True
isVal t | isNum t = True
isVal _           = False

eval' :: Expr -> Maybe Expr
eval' (IsZero Zero)               = Just Tr
eval' (IsZero (Succ t)) | isNum t = Just Fl
eval' (IsZero t)                  = IsZero <$> eval' t
eval' (Succ t)                    = Succ <$> eval' t
eval' (Pred Zero)                 = Just Zero
eval' (Pred (Succ t)) | isNum t   = Just t
eval' (Pred t)                    = Pred <$> eval' t
eval' (If Tr c _)                 = Just c
eval' (If Fl _ a)                 = Just a
eval' (If t c a)                  = (\t' -> If t' c a) <$> eval' t
eval' _                           = Nothing

eval :: Expr -> Maybe Expr
eval t =
  if isVal nft
    then Just nft
    else Nothing
 where
  nft = nf t
  nf x = fromMaybe x (nf <$> eval' x)
