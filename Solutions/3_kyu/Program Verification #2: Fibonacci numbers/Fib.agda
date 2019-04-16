{-# OPTIONS --safe #-}
module Fib where

open import Preloaded
open import Data.Nat
open import Relation.Binary.PropositionalEquality
open import Data.Nat.Properties using (+-comm; +-assoc)

lemma-aux : (f1 f2 f3 f4 n : ℕ) → fibAux (f3 + f1) (f4 + f2) n ≡ fibAux f3 f4 n + fibAux f1 f2 n
lemma-aux f1 f2 f3 f4 zero = refl
lemma-aux f1 f2 f3 f4 (suc n)
  rewrite sym (+-assoc (f3 + f1) f4 f2) | +-assoc f3 f1 f4 | +-comm f1 f4 | sym (+-assoc f3 f4 f1) | +-assoc (f3 + f4) f1 f2
    = lemma-aux f2 (f1 + f2) f4 (f3 + f4) n

fibEq : (n : ℕ) -> fibAux 0 1 n ≡ fib n
fibEq zero = refl
fibEq (suc zero) = refl
fibEq (suc (suc n)) rewrite lemma-aux 0 1 1 1 n | fibEq n | fibEq (suc n) = refl