module Pretty (
  ppexpr
) where

import Syntax

import Text.PrettyPrint

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

viewVars :: Expr -> [Name]
viewVars (Lam v b) = v : viewVars b
viewVars _         = []

viewBody :: Expr -> Expr
viewBody (Lam _ b) = viewBody b
viewBody e         = e

instance Pretty Expr where
  ppr _ (Lit (LInt x))  = int x
  ppr _ (Lit (LBool x)) = text (show x)
  ppr _ (Var x)         = text x
  ppr p (App f e)       = parensIf (p>0) $ ppr (p+1) f <+> ppr p e
  ppr p e@(Lam v b)     =
    parensIf (p>0) $
         char '\\'
      <> hsep (fmap text (viewVars e))
      <+> text "."
      <+> ppr (p+1) (viewBody e)

ppexpr :: Expr -> String
ppexpr = render . pp
