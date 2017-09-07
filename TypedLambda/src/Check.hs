module Check (
  checkExpr
  ) where

import           Pretty
import           Syntax
import           Type

import           Control.Monad.Except
import           Control.Monad.Reader

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name

instance Show TypeError where
  show (Mismatch a b) = "Type Mismatch: Got " ++ pprint a ++ " but expected " ++ pprint b
  show (NotFunction t) = "Not a function: " ++ pprint t
  show (NotInScope n) = "Variable not in scope: " ++ n

type Check = ExceptT TypeError (Reader Env)

type Env = [(Name, Type)]

extend :: (Name,Type) -> Env -> Env
extend xt env = xt:env

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: Name -> Check Type
lookupVar n = do
  env <- ask
  case lookup n env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope n

check :: Expr -> Check Type
check (Lit (LInt _)) = return TInt
check (Lit (LBool _)) = return TBool
check (Var n) = lookupVar n
check (App e1 e2) = do
  t1 <- check e1
  t2 <- check e2
  case t1 of
    (TArr a b)
      | a == t2 -> return b
      | otherwise -> throwError $ Mismatch t2 a
    _ -> throwError $ NotFunction t1
check (Lam n t e) = do
  et <- inEnv (n, t) (check e)
  return $ TArr t et

runCheck :: Env -> Check a -> Either TypeError a
runCheck env chk = runReader (runExceptT chk) env

checkExpr :: Expr -> Either TypeError Type
checkExpr = runCheck [] . check
