{-# OPTIONS --without-K --safe #-}
module Mult3 where

open import Data.Nat
open import Data.Nat.Properties
open import Relation.Binary.PropositionalEquality
open import Preloaded
open import Data.Nat.Properties using (+-comm; +-identityʳ)

PPP-mult : ∀ n → Mult3 (suc (suc (suc n))) → Mult3 n
PPP-mult n (SSS-mult .n m3_n) = m3_n

sum-Mult : ∀ m n → Mult3 m → Mult3 n → Mult3 (m + n)
sum-Mult .0 n 0-mult m3_n = m3_n
sum-Mult .(suc (suc (suc n₁))) n (SSS-mult n₁ m3_m) m3_n = SSS-mult (n₁ + n) (sum-Mult n₁ n m3_m m3_n)

diff-Mult : ∀ l n m → Mult3 n → Mult3 m → l + n ≡ m → Mult3 l
diff-Mult l .0 .(l + 0) 0-mult m3_m refl rewrite +-identityʳ l = m3_m
diff-Mult l .(suc (suc (suc n))) .(l + suc (suc (suc n))) (SSS-mult n m3_n) m3_m refl
  rewrite +-comm l (suc (suc (suc n))) | +-comm n l = diff-Mult l n (l + n) m3_n (PPP-mult (l + n) m3_m) refl

9-mult' : Mult3' 9
9-mult' = diff-mult 9 21 30 21-mult 30-mult refl

0-mult' : Mult3' 0
0-mult' = diff-mult zero 30 30 30-mult 30-mult refl

12-mult' : Mult3' 12
12-mult' = diff-mult 12 9 21 9-mult' 21-mult refl

3-mult' : Mult3' 3
3-mult' = diff-mult 3 9 12 9-mult' 12-mult' refl

mult-imp-mult' : ∀ {n : ℕ} → Mult3 n → Mult3' n
mult-imp-mult' 0-mult = 0-mult'
mult-imp-mult' (SSS-mult n m3_n) = sum-mult 3 n 3-mult' (mult-imp-mult' m3_n)

mult'-imp-mult : ∀ {n : ℕ} → Mult3' n → Mult3 n
mult'-imp-mult 30-mult = SSS-mult 27
                           (SSS-mult 24
                            (SSS-mult 21
                             (SSS-mult 18
                              (SSS-mult 15
                               (SSS-mult 12
                                (SSS-mult 9 (SSS-mult 6 (SSS-mult 3 (SSS-mult zero 0-mult)))))))))
mult'-imp-mult 21-mult = SSS-mult 18
                           (SSS-mult 15
                            (SSS-mult 12
                             (SSS-mult 9 (SSS-mult 6 (SSS-mult 3 (SSS-mult zero 0-mult))))))
mult'-imp-mult (sum-mult n m m3'_n m3'_m) = sum-Mult n m (mult'-imp-mult m3'_n) (mult'-imp-mult m3'_m)
mult'-imp-mult (diff-mult l n ._ m3'_n m3'_m refl) = diff-Mult l n (l + n) (mult'-imp-mult m3'_n) (mult'-imp-mult m3'_m)
                                                       refl