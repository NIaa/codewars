module Century where

century::Int -> Int
century year
  | mod year 100 == 0 = div year 100
  | otherwise         = div year 100 + 1