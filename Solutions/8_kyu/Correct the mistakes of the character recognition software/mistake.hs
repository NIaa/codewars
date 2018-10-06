module CodeWars.OCRMistakes where

correct :: String -> String
correct = map f where 
  f c
    | c=='5' = 'S'
    | c=='0' = 'O'
    | c=='1' = 'I'
    | otherwise = c