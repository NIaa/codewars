module Codewars.Kata.FakeBinary where

fakeBin :: String -> String
fakeBin xs = map (f) xs
  where f x | x < '5' = '0'
            | otherwise = '1'