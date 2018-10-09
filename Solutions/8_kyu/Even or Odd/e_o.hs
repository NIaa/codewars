module EvenOrOdd where

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n = if n `mod` 2 == 0 then "Even" else "Odd"