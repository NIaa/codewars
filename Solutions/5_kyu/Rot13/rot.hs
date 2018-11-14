module Rot13 where

import Data.Char

rot13 :: String -> String
rot13 = map rot where
  rot c | ord c >= ord 'a' && ord c <= ord 'z' = chr (ord 'a' + mod (ord c - ord 'a' + 13) 26)
        | ord c >= ord 'A' && ord c <= ord 'Z' = chr (ord 'A' + mod (ord c - ord 'A' + 13) 26)
        | otherwise = c