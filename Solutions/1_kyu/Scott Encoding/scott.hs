{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }
toPair :: SPair a b -> (a,b)
toPair (SPair f) = f (\a b -> (a, b))
fromPair :: (a, b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b)
fst :: SPair a b -> a
fst (SPair p) = p (\a _ -> a) 
snd :: SPair a b -> b
snd (SPair p) = p (\_ b -> b)
swap :: SPair a b -> SPair b a
swap sf = fromPair $ (\(a,b) -> (b,a)) (toPair sf)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f $ fromPair (a, b)
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \p -> f (fst p) (snd p) 

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe const
fromMaybe (Just a) = SMaybe (\b ab -> ab a)
isJust :: SMaybe a -> Bool
isJust m = isJust_M $ toMaybe m where 
  isJust_M Nothing = False
  isJust_M _ = True
isNothing :: SMaybe a -> Bool
isNothing m = isNothing_M $ toMaybe m where
  isNothing_M Nothing = True
  isNothing_M _ = False
catMaybes :: SList (SMaybe a) -> SList a
catMaybes lma = fromList [fromMaybe_P $ toMaybe ma | ma <- toList lma, isJust ma] where
  fromMaybe_P (Just x) = x

newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right 
fromEither :: Either a b -> SEither a b
fromEither (Left  a) = SEither (\ac bc -> ac a)
fromEither (Right b) = SEither (\ac bc -> bc b)
isLeft :: SEither a b -> Bool
isLeft  e = isLeft_E  $ toEither e where
  isLeft_E (Left _) = True
  isLeft_E _ = False
isRight :: SEither a b -> Bool
isRight e = isRight_E $ toEither e where
  isRight_E (Right _) = True 
  isRight_E _ = False
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition slse = fromPair (fromList la, fromList lb) where
  lse = toList slse
  fromLeft  (Left  a) = a
  fromRight (Right b) = b
  la = [fromLeft  $ toEither sa | sa <- lse, isLeft  sa]
  lb = [fromRight $ toEither sb | sb <- lse, isRight sb] 
  
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
toList :: SList a -> [a]
toList (SList l) = l [] (\a as -> a: toList as)
fromList :: [a] -> SList a
fromList [] = SList (\b a_sLa_b -> b)
fromList (x:xs) = SList (\b a_sLa_b -> a_sLa_b x (fromList xs))
cons :: a -> SList a -> SList a
cons a l = fromList $ a : (toList l)
concat :: SList a -> SList a -> SList a
concat l1 l2 = fromList $ toList l1 ++ toList l2 where
  (++) []     ys = ys
  (++) (x:xs) ys = x : xs ++ ys
null :: SList a -> Bool
null (SList f) = f True (\a as -> False)
length :: SList a -> Int
length la = length_P $ toList la
length_P [] = 0
length_P (_:xs) = 1 + length_P xs
map :: (a -> b) -> SList a -> SList b
map f la = fromList [f a | a <- (toList la)]
zip :: SList a -> SList b -> SList (SPair a b)
zip sla slb = fromList [fromPair p | p <- (zip_P la lb)] where
  la = toList sla
  lb = toList slb
  zip_P [] _ = []
  zip_P _ [] = []
  zip_P (x:xs) (y:ys) = (x, y) : zip_P xs ys
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl _ b la | null la = b
foldl f b la = foldl f (f b (ScottEncoding.head la)) (ScottEncoding.tail la)
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr _ b la | null la = b
foldr f b la = f (ScottEncoding.head la) (foldr f b (ScottEncoding.tail la))
take :: Int -> SList a -> SList a
take n la = fromList $ take_P n (toList la) where
  take_P n xs | n <= 0 = []
  take_P _ [] = []
  take_P n (x:xs) = x : take_P (n-1) xs
drop :: Int -> SList a -> SList a
drop n la = fromList $ drop_P n (toList la) where
  drop_P n xs | n <= 0 = xs
  drop_P _ [] = []
  drop_P n (x:xs) = drop_P (n-1) xs
head :: SList a -> a
head la = head_P $ toList la where 
  head_P (x:_) = x
tail :: SList a -> SList a
tail la = fromList $ tail_P $ toList la where
  tail_P (_:xs) = xs