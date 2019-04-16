{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Counting where
import Counting.Preloaded
import Data.Proxy
import Data.Void
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose
import Data.Maybe (fromMaybe)
import Control.Monad (filterM)
import Data.Coerce (coerce)

{- in Preloaded:
data Nat = Z | S Nat deriving (Show, Eq, Ord)
instance Num Nat -- so (2 :: Nat) == S (S Z)
instance Enum Nat -- so ([0..3] :: [Nat]) == [Z, S Z, S (S Z)]
instance Real Nat
instance Integral Nat -- so (2 ^ 3 :: Nat) == S (S (S (S (S (S (S (S Z)))))))
-}

infinity :: Nat
infinity = S infinity

newtype Count x = Count { getCount :: Nat } deriving (Show, Eq, Ord)

-- | helper functions
mapC :: (Nat -> Nat) -> Count a -> Count b
mapC f (Count a) = Count $ f a

liftC2 :: (Nat -> Nat -> Nat) -> Count a -> Count b -> Count c
liftC2 f (Count a) (Count b) = Count $ f a b

coerceC :: Count a -> Count b
coerceC = coerce

-- | Countable
class Countable c where
  count :: Count c
  count = count' (Proxy :: Proxy c)
  count' :: Proxy c -> Count c
  count' _ = count :: Count c
  
  
instance Countable Void where count = Count Z
instance Countable () where count = Count $ S Z
instance Countable Bool where count = Count $ S (S Z)
instance Countable Nat where count = Count infinity

-- | Factor
class Factor f where
  factor :: Count c -> Count (f c)
  factor = factor' (Proxy :: Proxy f)
  factor' :: Proxy f -> Count c -> Count (f c)
  factor' _ = factor :: (Count c -> Count (f c))
  
instance (Factor f, Countable c) => Countable (f c) where
  count = factor (count :: Count c)

instance Factor Maybe where 
  factor = mapC S
instance Factor Identity where 
  factor = mapC id
instance Factor Proxy where 
  factor _ = Count $ S Z
instance Factor Count where 
  factor _ = Count infinity
instance Factor [] where
  factor (Count Z) = Count $ S Z
  factor _ = Count infinity
instance Countable c => Factor (Const c) where 
  factor = liftC2 const (count :: Count c)
instance Countable c => Factor (Either c) where 
  factor = liftC2 (+) (count :: Count c)
instance Countable c => Factor ((,) c) where 
  factor = liftC2 (*) (count :: Count c)
instance Countable c => Factor ((->) c) where 
  factor (Count c) = mapC (c ^) (count :: Count c)
instance (Factor f, Factor g) => Factor (Sum f g) where
  factor c = liftC2 (+) (factor @f c) (factor @g c)
instance (Factor f, Factor g) => Factor (Product f g) where 
  factor c = liftC2 (*) (factor @f c) (factor @g c)
instance (Factor f, Factor g) => Factor (Compose f g) where
  factor = coerce . factor @f . factor @g
  
-- | Listable
class Countable a => Listable a where
  list :: [a]
  -- list' :: Proxy a -> [a] -- optional
-- Data.List.genericLength (list :: [a]) `shouldBe` getCount (count :: Count a)

instance Listable Void where 
  list = []
instance Listable () where 
  list = [()]
instance Listable Bool where 
  list = [False, True] 
instance Listable Nat where 
  list = iterate S Z
--   list = Z : fmap S list
--   list = [0..]
instance Listable c => Listable (Maybe c) where 
  list = Nothing : fmap Just list
instance Listable c => Listable [c] where 
--   list = filterM (const [True, False]) list
  list = [] : [(l1 : l2) | l1 <- list, l2 <- list]
instance (Listable a, Listable b) => Listable (Either a b) where 
  list = map Left (list :: [a]) ++ map Right (list :: [b])
instance (Listable a, Listable b) => Listable (a, b) where 
  list = [(a, b) | a <- list :: [a], b <- list :: [b]]

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where 
  list = map toFunc $ listPower list list where
    toFunc as a = fromMaybe (error "") $ lookup a as
    listPower [] bs = [[]]
    listPower (a:as) bs = (:) <$> map ((,) a) bs <*> listPower as bs
