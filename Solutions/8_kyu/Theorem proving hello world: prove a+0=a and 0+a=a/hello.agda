{-# OPTIONS --safe #-}
module 8kyu where

-- Define a datatype
data Nat : Set where
  -- which can be a zero
  Z : Nat
  -- or the successor of another `Nat`
  S : Nat -> Nat

-- Two is the successor of the successor of zero
two : Nat
two = S (S Z)

-- So we can find the predeccessor
pred : Nat -> Nat
-- we can investigate the input `Nat`: is it zero?
-- if so, return zero
pred Z = Z
-- .. or is it the successor of another `Nat`?
-- we call the other nat `n`.
pred (S n) =
  -- `n`'s successor's predeccessor is obviously `n` itself
  n

-- plus takes two nats
plus : Nat -> Nat -> Nat
-- if the first nat is zero, return the second nat
plus Z n = n
-- otherwise the first nat must be the successor of someone `n`.
-- we recursively call `plus` by `plus m n`
plus (S n) m = let recursiveResult = plus n m
               -- and return the successor.
               in S recursiveResult

-- equality type
-- oh, so f* complicated, let's just see how to use it
data Eq {A : Set} (x : A) : A -> Set where
  -- a constructor whose type is `Eq x x`
  refl : Eq x x

-- So we get reflexivity for free
-- everything is equivalent to itself

-- specialize `Eq x x` to `Eq (S Z) (S Z)` can be
freeOhYeah : Eq (S Z) (S Z)
freeOhYeah = refl

-- And we can write more complex things like
evenMoreC : Eq (plus two Z) (plus Z two)
-- Agda will compute the value of `plus two Z` and `plus Z two`
-- the results are identical so `refl`
evenMoreC = refl

-- The most useful proving helper function: cong
cong : {n m : Nat} ->
       -- this is a specialized version
       -- because I want to keep everything simple
       (f : Nat -> Nat) ->
       Eq n m -> Eq (f n) (f m)
-- if we pattern match `Eq n m` with `refl`, Agda will try to unify
-- two statements: `Eq x x` and `Eq n m`, resulting `n` and `m` are identical
cong f refl =
  -- Agda keeps this in mind, and replace `m` with `n` in the context.
  -- Now the return type becomes `Eq (f n) (f n)`, which is yet another
  -- specialized `refl`.
  refl

-- And we can prove this trivially
-- because `plus` is defined with `plus Z n = n`
0+a : (n : Nat) -> Eq (plus Z n) n
-- so Agda replaces `plus Z n` with `n`
-- making this super easy to prove
0+a n = refl

-- And we can prove this in a little bit more complex way
a+0 : (n : Nat) -> Eq (plus n Z) n
-- because `plus` is *not* defined with `plus n Z`
-- how do we prove this?
a+0 Z = refl
a+0 (S n) = cong S (a+0 n)

-- by using something you've already learnt