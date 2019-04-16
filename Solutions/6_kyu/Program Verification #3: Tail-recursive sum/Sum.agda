{-# OPTIONS --safe #-}
module Sum where

open import Data.Nat
open import Data.Nat.Properties
open import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)
open import Preloaded

lemma : (acc : ℕ) → (f : ℕ → ℕ) → (n : ℕ) → sumAux acc f n ≡ acc + sumSimple f n
lemma acc f zero = +-comm (f zero) acc
lemma acc f (suc n) = sumAux acc f (suc n)
  ≡⟨ lemma (f (suc n) + acc) f n ⟩ f (suc n) + acc + sumSimple f n
  ≡⟨ cong (λ x → x + sumSimple f n) (+-comm (f (suc n)) acc) ⟩ acc + f (suc n) + sumSimple f n
  ≡⟨ +-assoc acc (f (suc n)) (sumSimple f n) ⟩ acc + (f (suc n) + sumSimple f n)
  ∎

sumEq : (f : ℕ → ℕ) → (n : ℕ) → sumTail f n ≡ sumSimple f n
sumEq f zero = +-identityʳ (f zero) 
sumEq f (suc n) = sumTail f (suc n)
  ≡⟨ cong (λ x → sumAux x f n) (+-identityʳ (f (suc n))) ⟩ sumAux (f (suc n)) f n
  ≡⟨ lemma (f (suc n)) f n ⟩ f (suc n) + sumSimple f n
  ∎