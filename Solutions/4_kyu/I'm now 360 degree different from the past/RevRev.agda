{-# OPTIONS --safe #-}
module RevRev where

open import Relation.Binary.PropositionalEquality
open import Data.List
open import Rev

lemma : ∀ {ℓ} {A : Set ℓ} (xs : List A) (x : A) → rev (xs ++ x ∷ []) ≡ x ∷ rev xs
lemma [] x = refl
lemma (x₁ ∷ xs) x = cong (λ x → x ++ x₁ ∷ []) (lemma xs x)

revrevid : ∀ {ℓ} {A : Set ℓ} (a : List A) → rev (rev a) ≡ a
revrevid [] = refl
revrevid (x ∷ xs) = trans (lemma (rev xs) x) (cong (_∷_ x) (revrevid xs))