module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where

import Control.Monad.State
import Data.Map

type Env = Map Int Integer
type SomeMonad a = State Env a

newtype Var = Var Int
newtype Lit = Lit Integer

class SomeVariable v where
    eval :: v -> Env -> Integer

instance SomeVariable Lit where
    eval (Lit n) _ = n

instance SomeVariable Var where
    eval (Var i) env = env ! i

def :: SomeVariable v => SomeMonad v -> Integer
def m = eval v env where (v, env) = runState m empty

var :: Integer -> SomeMonad Var
var v = do env <- get
           let l = length env
           put $ insert l v env
           return $ Var l

lit :: Integer -> Lit
lit = Lit

while :: SomeVariable v => v -> (Integer -> Bool) -> SomeMonad () -> SomeMonad ()
while v cond action = do
    env <- get
    let n = eval v env
    when (cond n) (put (snd $ runState action env) >> while v cond action)

opAssign :: SomeVariable v => (Integer -> Integer -> Integer) -> Var -> v -> SomeMonad ()
opAssign op (Var i) v = do
    env <- get
    put $ insert i (env ! i `op` eval v env) env

(+=) :: SomeVariable v => Var -> v -> SomeMonad ()
(+=) = opAssign (+)
(-=) :: SomeVariable v => Var -> v -> SomeMonad ()
(-=) = opAssign (-)
(*=) :: SomeVariable v => Var -> v -> SomeMonad ()
(*=) = opAssign (*)