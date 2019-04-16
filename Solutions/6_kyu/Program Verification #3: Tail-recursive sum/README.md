<https://www.codewars.com/kata/program-verification-number-3-tail-recursive-sum/train/agda>   
Use lemma locally simplifies a lot. (With rewrite
```
{-# OPTIONS --safe #-}
module Sum where

open import Data.Nat
open import Data.Nat.Properties
open import Relation.Binary.PropositionalEquality
open import Preloaded

sumEq : (f : ℕ → ℕ) → (n : ℕ) → sumTail f n ≡ sumSimple f n
sumEq f n = lemma 0 n
  where
    lemma : ∀ (a n) → sumAux a f n ≡ a + sumSimple f n
    lemma a zero = +-comm (f 0) a
    lemma a (suc n) rewrite lemma (f (suc n) + a) n
      | +-comm (f (suc n)) a 
      | +-assoc a (f (suc n)) (sumSimple f n) = refl
```