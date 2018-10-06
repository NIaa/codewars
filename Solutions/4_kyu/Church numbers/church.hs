{-# LANGUAGE RankNTypes #-}

module Haskell.Codewars.Church where

import Prelude hiding (succ, pred, and, or, not, fst, snd, head, tail)

add :: Number -> Number -> Number
--add (Nr a) = undefined
add (Nr a) (Nr b) = Nr (\f -> (a f) . (b f))

mult :: Number -> Number -> Number
mult (Nr a) (Nr b) = Nr (a . b)

pow :: Number -> Number -> Number
pow (Nr x) (Nr n) = Nr (n x)

-- cannot be placed in Preloaded right now as there is a bug in the codewars system

instance Num Number where
  fromInteger n
    | n < 0 = error "cannot convert negative numbers"
    | n == 0 = zero
    | otherwise  = succ (fromInteger $ n-1)
  a + b = add a b
  a - b = error "not implemented"
  a * b = mult a b
  abs _ = error "not implemented"
  signum _ = error "not implemented"
  
newtype Number = Nr (forall a. (a -> a) -> a -> a)

instance Show Number where
  show (Nr a) = a ("1+" ++) "0"

eval :: Number -> Integer
eval (Nr a) = a (+1) 0

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ s z -> s (a s z))

one :: Number
one = succ zero

two :: Number
two = succ one

three :: Number
three = succ two

four :: Number
four = succ three

five :: Number
five = succ four