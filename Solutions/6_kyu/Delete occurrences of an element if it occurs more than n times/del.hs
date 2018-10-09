module Codewars.Kata.Deletion where

deleteNth :: [Int] -> Int -> [Int]
deleteNth xs n = foldl step [] xs
  where
    step ys x | (length . filter(==x)) ys < n = ys ++ [x]
              | otherwise = ys