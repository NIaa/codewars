{-# OPTIONS --safe #-}
module FlipTreeSym where

open import Relation.Binary.PropositionalEquality
open import Preloaded

flipTreeSym : {A : Set} → (t : Tree A) → t ≡ flipTree (flipTree t)
flipTreeSym (leaf x) = refl
flipTreeSym (branch x t t₁) = trans (cong (λ p → branch x t p) (flipTreeSym t₁)) (cong (λ p → branch x p (flipTree(flipTree t₁))) (flipTreeSym t))