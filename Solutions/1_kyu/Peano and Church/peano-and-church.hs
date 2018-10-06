{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List
import Data.Maybe

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (\aaa -> \b1 b2 -> ab $ aaa (ba b1) (ba b2), 
                     \bbb -> \a1 a2 -> ba $ bbb (ab a1) (ab a2))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

data Peano = O | S Peano deriving (Show, Eq, Ord)
instance Nat Peano where
  zero = O
  successor n = S n
  nat a _ O = a
  nat _ f (S n) = f n
  iter a _ O = a
  iter a f (S n) = f (iter a f n)
  plus O n = n
  plus (S m) n = S (plus m n)
  minus m O = m
  minus O _ = O
  minus (S m) (S n) = minus m n
  mult O n = O
  mult (S m) n = plus (mult m n) n
  pow _ O = S O
  pow O _ = O
  pow m (S n) = mult (pow m n) m

instance Nat [()] where
  zero = []
  successor n = () : n
  nat a _ [] = a
  nat _ f (_:xs) = f xs
  iter a _ [] = a
  iter a f (_:xs) = f (iter a f xs)
  plus [] xs = xs
  plus (_:xs) ys = () : (plus xs ys)
  minus xs [] = xs
  minus [] _ = []
  minus (_:xs) (_:ys) = minus xs ys
  mult [] _ = []
  mult (x:xs) ys = (mult xs ys) ++ ys
  pow _ [] = [()]  
  pow [] _ = []
  pow xs (_:ys) = mult (pow xs ys) xs
  
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  zero = Scott const
  successor n = Scott (\_ f -> f n)
  nat a f (Scott n)  = n a f
  iter a f (Scott n) = n a $ iter (f a) f
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }

predecessor :: Church -> Church
predecessor (Church n) = Church (\ f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u))  
isZero :: Church -> Bool
isZero (Church n) = n (const False) True

instance Nat Church where
  zero = Church (\ _ z -> z)
  successor (Church a) = Church (\ s z -> s (a s z)) 
  nat d f a | isZero a = d
  nat _ f a = f (predecessor a)
  iter d f (Church a) = a f d
  plus (Church a) (Church b) = Church (\f -> (a f) . (b f)) 
  minus a (Church b) = b predecessor a
  mult (Church a) (Church b) = Church (a . b)
  pow (Church x) (Church n) = Church (n x)