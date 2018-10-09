module Average where

avg :: [Float] -> Float
avg l = (sum l) / (sum [1 | _ <- l])
