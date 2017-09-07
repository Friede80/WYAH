module Eval (
  runEval
) where

import           Syntax

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as Map

data EvalState = EvalState
  { depth :: Int
  } deriving Show

type Step = (Int, Expr)

type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map.Map Name Value

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr Scope

instance Show Value where
  show (VInt x)   = show x
  show (VBool x)  = show x
  show VClosure{} = "<<closure>>"

red :: Expr -> Eval ()
red x = do
  d <- gets depth
  tell [(d,x)]
  return ()

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s{ depth = depth s + 1}
  out <- m
  modify $ \s -> s{ depth = depth s - 1}
  return out

eval :: Scope -> Expr -> Eval Value
eval _   (Lit (LInt x)) = return $ VInt (fromIntegral x)
eval _   (Lit (LBool x)) = return $ VBool x
eval env e@(Var x) = do
  red e
  return $ env Map.! x
eval env (Lam v b) = inc $ return (VClosure v b env)
eval env (App f e) = inc $ do
  x <- eval env f
  red f
  y <- eval env e
  red e
  apply x y

apply :: Value -> Value -> Eval Value
apply (VClosure v e1 env) e2 = eval (extend env v e2) e1
apply _ _                    = error "Tried to apply non-closure"

extend :: Scope -> Name -> Value -> Scope
extend env v  t = Map.insert v t env

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> (Value, [Step])
runEval e = evalState (runWriterT (eval emptyScope e)) (EvalState 0)
