{-# OPTIONS --safe #-}
module OddsAndEvens where

open import Preloaded
open import Data.Nat

{- Preloaded functions:
_e+e_ : ∀ {m n : ℕ} → Even m → Even n → Even (m + n)
_o+e_ : ∀ {m n : ℕ} → Odd  m → Even n → Odd  (m + n)
_o+o_ : ∀ {m n : ℕ} → Odd  m → Odd  n → Even (m + n)
_e+o_ : ∀ {m n : ℕ} → Even m → Odd  n → Odd  (m + n)
-}

-- | Implement these functions:
infixl 7 _e*e_ _o*e_ _o*o_ _e*o_
_e*e_ : ∀ {m n : ℕ} → Even m → Even n → Even (m * n)
_o*e_ : ∀ {m n : ℕ} → Odd  m → Even n → Even (m * n)
_o*o_ : ∀ {m n : ℕ} → Odd  m → Odd  n → Odd  (m * n)
_e*o_ : ∀ {m n : ℕ} → Even m → Odd  n → Even (m * n)

zero e*e y = zero
suc x e*e y = y e+e (x o*e y)
suc x o*e y = y e+e (x e*e y)
suc zero o*o y = y o+e zero
suc (suc x) o*o y = y o+e (suc x e*o y)
zero e*o y = zero
suc x e*o y = y o+o (x o*o y)