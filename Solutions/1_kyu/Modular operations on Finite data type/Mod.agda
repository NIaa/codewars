{-# OPTIONS --safe #-}
module Mod where

open import Data.Fin using (Fin; zero; suc; toℕ; fromℕ; inject₁; inject≤)
  
open import Data.Nat as ℕ using (ℕ; zero; suc; z≤n; s≤s ; _+_; _≤_)
open import Data.Nat.DivMod using (_%_; a%n<n)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

variable k : ℕ

last : Fin (suc k)
last {k = zero} = zero
last {k = suc _} = suc last

addtoℕ : ∀ (d : ℕ) → Fin d → Fin d → ℕ
addtoℕ zero m n = zero
addtoℕ (suc d) m n = (toℕ m + toℕ n) % (suc d)

add : Fin k -> Fin k -> Fin k
add {suc k} m n = inject≤ (fromℕ (addtoℕ (suc k) m n)) (a%n<n (toℕ m + toℕ n) k)

negate : Fin k -> Fin k
negate zero = zero
negate (suc zero) = last
negate (suc n) = inject₁ (negate n)

subt : Fin k -> Fin k -> Fin k
subt n m = add n (negate m)

-- for termination check
mult-helper : ∀ {m n} → Fin m → Fin n → Fin n
mult-helper zero zero = zero
mult-helper zero (suc fn) = zero
mult-helper (suc fm) zero = zero
mult-helper (suc fm) (suc fn) = add (suc fn) (mult-helper fm (suc fn))

mult : Fin k -> Fin k -> Fin k
mult n m = mult-helper n m