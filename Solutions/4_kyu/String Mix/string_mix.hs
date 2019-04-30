module Codewars.G964.Mixin where

import GHC.Exts (sortWith)
import Data.Char (ord)

count_lower :: [Char] -> [(Char, Int)]
count_lower xs = [(x, length $ filter (x==) xs) | x <- ['a'..'z']]

cmp :: (Char, Int) -> (Char, Int) -> ([Char], [Char])
cmp (c, n1) (_, n2) | n1 <= 1 && n2 <= 1 = (" ", " ")
                    | n1 > n2   = ("1:", replicate n1 c)
                    | n1 < n2   = ("2:", replicate n2 c)
                    | otherwise = ("=:", replicate n1 c)

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = case result of 
  "" -> ""
  _  -> init result
  where
    lc = sortWith helper_s (zipWith cmp (count_lower s1) (count_lower s2))
    helper_f (" ", _) b = b
    helper_f (s1, s2) b = s1 ++ s2 ++ "/" ++ b
    helper_s (s1, s2) = negate $ 1024 * (length s2) - ord (head s2) - 32 * ord (head s1)
    result = foldr helper_f "" lc