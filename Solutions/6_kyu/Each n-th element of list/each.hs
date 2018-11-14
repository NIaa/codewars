module Each where

each :: Int -> [Int] -> [Int]
each 0 _ = []
each n xs
    | abs n > length xs = []
    | n > 0             = xs!!(n-1) : each n (drop n xs)
    | otherwise         = xs!!(length xs + n) : each n (take (length xs + n) xs)
