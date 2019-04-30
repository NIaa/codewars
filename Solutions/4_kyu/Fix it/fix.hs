module Fixit where
import Prelude hiding (reverse, foldr)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' rec as = if null as then [] else rec (tail as) ++ [head as]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' rec f acc xs = if null xs then acc else f (head xs) (rec f acc (tail xs))