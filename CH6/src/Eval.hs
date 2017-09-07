module Eval (
  runEval
) where

import           Syntax

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as Map

type Scope = Map.Map Name Value

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr Scope

instance Show Value where
  show (VInt x)   = show x
  show (VBool x)  = show x
  show VClosure{} = "<<closure>>"

eval :: Scope -> Expr -> Value
eval _   (Lit (LInt x)) = VInt $ fromIntegral x
eval _   (Lit (LBool x)) = VBool x
eval env (Prim op e1 e2) = evalPrim op (eval env e1) (eval env e2)
eval env e@(Var x) = env Map.! x
eval env (Lam v _ b) = VClosure v b env
eval env (App f e) =
  let x = eval env f
      y = eval env e
  in apply x y

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt x) (VInt y) = VInt $ x + y
evalPrim Mul (VInt x) (VInt y) = VInt $ x * y
evalPrim _ _ _ = error "Primitive operation on non-Int"

apply :: Value -> Value -> Value
apply (VClosure v e1 env) e2 = eval (extend env v e2) e1
apply _ _                    = error "Tried to apply non-closure"

extend :: Scope -> Name -> Value -> Scope
extend env v  t = Map.insert v t env

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> Value
runEval = eval emptyScope
