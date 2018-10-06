module Codewars.G964.Operarray where

gcdi :: Integer -> Integer -> Integer
gcdi = gcd
lcmu :: Integer -> Integer -> Integer
lcmu = lcm
som :: Integer -> Integer -> Integer
som = (+)
maxi :: Integer -> Integer -> Integer
maxi = max
mini :: Integer -> Integer -> Integer
mini = min 

operArray :: (Integer -> Integer -> Integer) -> [Integer] -> Integer -> [Integer]
operArray _ [] _ = []
operArray fct (x:xs) init = next : operArray fct xs next where
  next = fct x init