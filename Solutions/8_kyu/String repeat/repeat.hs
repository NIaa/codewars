module StringRepeat where

repeatStr :: Int -> String -> String
repeatStr n str = [c|n<-[1..n],c<-str]