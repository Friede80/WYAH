module Pretty (
  pprint
) where

import Syntax
import Type

import Text.PrettyPrint

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

  pprint :: p -> String
  pprint = render . ppr 0

viewVars :: Expr -> [Name]
viewVars (Lam n _ e) = n : viewVars e
viewVars _         = []

viewBody :: Expr -> Expr
viewBody (Lam _ _ b) = viewBody b
viewBody e         = e

instance Pretty Expr where
  ppr _ (Lit (LInt x))  = int x
  ppr _ (Lit (LBool x)) = text (show x)
  ppr _ (Var x)         = text x
  ppr p (App f e)       = parensIf (p>0) $ ppr (p+1) f <+> ppr p e
  ppr p l@(Lam _ _ e)   =
    parensIf (p>0) $
         char '\\'
      <> hsep (fmap text (viewVars l))
      <+> text "->"
      <+> ppr (p+1) (viewBody e)

instance Pretty Type where
  ppr _ TInt = text "Int"
  ppr _ TBool = text "Bool"
  ppr p (TArr a b) = parensIf (p > 0) $ ppr (p+1) a <+> text "->" <+> ppr (p+1) b
