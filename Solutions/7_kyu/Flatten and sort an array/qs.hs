module FlatSort where

flatSort :: [[Int]] -> [Int]
flatSort = qs . concat 

qs :: [Int] -> [Int]
qs l | length l <= 1 = l
qs l = qs [x | x <- tail l, x <= head l] ++ [head l] ++ qs [x | x <- tail l, x > head l]