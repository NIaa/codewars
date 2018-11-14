module Fibonacci where

fibonacci :: Int -> Integer
fibonacci = (!!) fib where 
fib = 0 : 1 : zipWith (+) fib (tail fib)