{-# OPTIONS --safe #-}
module Proof where

open import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)
open import Data.Nat
open import Data.Nat.Properties using (*-identityˡ; *-cancelˡ-≡; +-identityʳ)

double : (a : ℕ) → a + a ≡ 2 * a
double a rewrite +-identityʳ a = refl

lemma : (a b : ℕ) → a + a ≡ b + b → 2 * a ≡ 2 * b
lemma a b e = 2 * a
  ≡⟨ sym (double a) ⟩ a + a
  ≡⟨ e ⟩ b + b
  ≡⟨ double b ⟩ b + (b + zero)
  ∎

invert : (a b : ℕ) → a + a ≡ b + b → a ≡ b
invert a b e = *-cancelˡ-≡ 1 (lemma a b e)

---------------------------------------------------------------------------- 
{-# OPTIONS --safe #-}
module Proof where

open import Relation.Binary.PropositionalEquality as Eq
open import Data.Nat
open import Data.Nat.Properties using (+-comm)

invert : (a b : ℕ) → a + a ≡ b + b → a ≡ b
invert zero zero e = refl
invert zero (suc b) ()
invert (suc a) zero ()
invert (suc a) (suc b) e = cong suc (invert a b helper) where
  helper : a + a ≡ b + b
  helper rewrite +-comm a (suc a) | +-comm b (suc b) = cong ((λ x → pred (pred x))) e