{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Counting where
import Counting.Preloaded
import Data.Proxy
import Data.Void
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose

{- in Preloaded:
data Nat = Z | S Nat deriving (Show, Eq, Ord)
instance Num Nat -- so (2 :: Nat) == S (S Z)
instance Enum Nat -- so ([0..3] :: [Nat]) == [Z, S Z, S (S Z)]
instance Real Nat
instance Integral Nat -- so (2 ^ 3 :: Nat) == S (S (S (S (S (S (S (S Z)))))))
-}

newtype Count x = Count { getCount :: Nat } deriving (Show, Eq, Ord)

-- | helper functions
mapC :: (Nat -> Nat) -> Count a -> Count b
mapC = error "todo: optional"

liftC2 :: (Nat -> Nat -> Nat) -> Count a -> Count b -> Count c
liftC2 = error "todo: optional"

coerceC :: Count a -> Count b
coerceC = error "todo: optional"

-- | Countable
class Countable c where
  count :: Count c
  -- if you are using `Proxy` implement `count` from `count'` and vice versa
  -- count' :: Proxy c -> Count c
  -- count' = error "from count"

instance Countable Void where count = undefined
instance Countable () where count = undefined
instance Countable Bool where count = undefined
instance Countable Nat where count = undefined

-- | Factor
class Factor f where
  factor :: Count c -> Count (f c)
  -- factor' :: Proxy f -> Count c -> Count (f c) -- optional

instance (Factor f, Countable c) => Countable (f c) where
  count = error "of course it is not useless"

instance Factor Maybe where factor = undefined
instance Factor Identity where factor = undefined
instance Factor Proxy where factor = undefined
instance Factor Count where factor = undefined
instance Factor [] where factor = undefined
instance Countable c => Factor (Const c) where factor = undefined
instance Countable c => Factor (Either c) where factor = undefined
instance Countable c => Factor ((,) c) where factor = undefined
instance Countable c => Factor ((->) c) where factor = undefined
instance (Factor f, Factor g) => Factor (Sum f g) where factor = undefined
instance (Factor f, Factor g) => Factor (Product f g) where factor = undefined
instance (Factor f, Factor g) => Factor (Compose f g) where factor = undefined

-- | Listable
class Countable a => Listable a where
  list :: [a]
  -- list' :: Proxy a -> [a] -- optional
-- Data.List.genericLength (list :: [a]) `shouldBe` getCount (count :: Count a)

instance Listable Void where list = undefined
instance Listable () where list = undefined
instance Listable Bool where list = undefined
instance Listable Nat where list = undefined

instance Listable c => Listable (Maybe c) where list = undefined
instance Listable c => Listable [c] where list = undefined
instance (Listable a, Listable b) => Listable (Either a b) where list = undefined
instance (Listable a, Listable b) => Listable (a, b) where list = undefined
instance (Eq a, Listable a, Listable b) => Listable (a -> b) where list = undefined