module SF37 where

houseNumbersSum :: [Int] -> Int
houseNumbersSum (x:xs) | x==0 = 0
                       | otherwise = x + (houseNumbersSum xs)