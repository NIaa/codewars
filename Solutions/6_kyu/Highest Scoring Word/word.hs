module High.JorgeVS.Kata where
high :: String -> String
high "" = ""
high s = snd $ maximum $ zip (map score w) w where 
  w = words s
  score "" = 0
  score (x:xs) = fromEnum x - fromEnum 'a' + 1 + score xs