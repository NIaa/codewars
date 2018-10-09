module Codewars.Kata.SplitStrings where

solution :: String -> [String]
solution "" = []
solution [x] = [[x, '_']]
solution (x1:x2:xs) = [x1, x2] : solution xs