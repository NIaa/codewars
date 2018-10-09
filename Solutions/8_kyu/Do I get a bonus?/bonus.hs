module Bonus where

iHazBonus :: Float->  Bool -> String

iHazBonus s b
  | b == True = '$' : show (10 * s)
  | otherwise = '$' : show s