{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module PolyvariadicFunctions where

-- https://wiki.haskell.org/Functional_dependencies
-- https://github.com/AJFarmar/haskell-polyvariadic

class SumReturnType a where
    retSum :: Int -> a

instance SumReturnType Int where
    retSum = id

instance (Integral a, SumReturnType r) => SumReturnType (a -> r) where
    retSum m n = retSum $ m + (fromIntegral n)

-- Works locally (ghc 8.4.3), so what {-# LANGUAGE #-} becomes default?
-- instance SumReturnType r => SumReturnType (Integer -> r) where
--     retSum m n = retSum (m + fromInteger n)


polyAdd :: SumReturnType r => r
polyAdd = retSum 0

class ListReturnType a r | r -> a where
  retList :: [a] -> r

instance ListReturnType a [a] where
  retList = id
  
instance (ListReturnType a r) => ListReturnType a (a -> r) where
  retList xs x = retList (xs ++ [x])

polyList :: (ListReturnType a r) => r
polyList = retList []

class WordsReturnType r where
  retWords :: String -> r

instance WordsReturnType String where
  retWords = id

instance (WordsReturnType r) => WordsReturnType (String -> r) where
  retWords "" s2 = retWords s2
  retWords s1 s2 = retWords (s1 ++ " " ++ s2)

polyWords :: (WordsReturnType r) => r
polyWords = retWords ""