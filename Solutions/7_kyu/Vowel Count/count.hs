module Codewars.Kata.Vowel where

getCount :: String -> Int
getCount s = length [c| c<-s, elem c "aeiou"] 