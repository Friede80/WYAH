module Pretty (
  pprint
) where

import Syntax
import Type

import Text.PrettyPrint (Doc, (<>), (<+>), render, text, parens)

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

  pprint :: p -> String
  pprint = render . ppr 0

instance Pretty Expr where
  ppr _ Zero = text "0"
  ppr _ Tr = text "true"
  ppr _ Fl = text "false"
  ppr p (Succ a) = parensIf (p > 0) $ text "succ" <+> ppr (p+1) a
  ppr p (Pred a) = parensIf (p > 0) $ text "succ" <+> ppr (p+1) a
  ppr p (IsZero a) = parensIf (p > 0) $ text "iszero" <+> ppr (p+1) a
  ppr p (If a b c) =
        text "if"   <+> ppr p a
    <+> text "then" <+> ppr p b
    <+> text "else" <+> ppr p c

instance Pretty Type where
  ppr _ TNat = text "Nat"
  ppr _ TBool = text "Bool"
  ppr p (TArr a b) = parensIf (p > 0) $ ppr (p+1) a <+> text "->" <+> ppr (p+1) b
