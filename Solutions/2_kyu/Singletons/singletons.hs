{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero b = b
type instance Add (Succ a) b = Succ (Add a b)

-- implement of hint
type family (Sub (a :: Nat) (b :: Nat)) :: Nat
type instance Sub a Zero = a
type instance Sub Zero b = Zero
type instance Sub (Succ a) (Succ b) = Sub a b

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min Zero b = Zero
type instance Min a Zero = Zero
type instance Min (Succ a) (Succ b) = Succ (Min a b)

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x xs) = x
index (SSucc a) (VCons x xs) = index a xs

replicate :: s -> SNat a -> Vec s a
replicate s SZero = VNil
replicate s (SSucc a) = VCons s (replicate s a) 

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _ _ VNil = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ ys = ys
(VCons x xs) ++ ys = VCons x (xs ++ ys)

-- The semantics should match that of take for normal lists.
take :: SNat a -> Vec v n -> Vec v (Min a n)
take SZero xs = VNil
take _ VNil = VNil
take (SSucc a) (VCons x xs) = VCons x (take a xs) 

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec v n -> Vec v (Sub n a)
drop SZero xs = xs
drop _ VNil = VNil
drop (SSucc a) (VCons x xs) = drop a xs

head :: Vec v n -> v
head (VCons x xs) = x

tail :: Vec v (Succ n) -> Vec v n
tail (VCons x xs) = xs