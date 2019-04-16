module Fibonacci where

fib:: Int -> Integer
fib = (!!) $ 0 : fn where 
  fn = 0 : 1 : zipWith (+) fn (tail fn)