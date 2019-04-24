module Razor where

data Razor
  = Lit Int
  | Add Razor Razor
  
interpret :: Razor -> Int
interpret (Lit n) = n
interpret (Add r1 r2) = (interpret r1) + (interpret r2)

pretty :: Razor -> String
pretty r = case r of
  Lit n -> show n
  Add r1 r2 -> "(" ++ pretty r1 ++ "+" ++ pretty r2 ++ ")"