{-# OPTIONS --safe #-}
module PerfectSquare where

open import Data.Nat
open import Data.Nat.Properties
open import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)
-- plfa

expand : ∀ n -> (n + 1) ^ 2 ≡ n ^ 2 + 2 * n + 1
expand n =
  begin
    (n + 1) ^ 2
  ≡⟨⟩
    (n + 1) * ((n + 1) ^ 1)
  ≡⟨ cong (_*_ (n + 1)) (^-identityʳ (n + 1)) ⟩
    (n + 1) * (n + 1)
  ≡⟨ *-distribʳ-+ (n + 1) n 1 ⟩
    n * (n + 1) + 1 * (n + 1)
  ≡⟨ cong (_+_ (n * (n + 1))) (*-identityˡ (n + 1)) ⟩
    n * (n + 1) + (n + 1)
  ≡⟨ cong (λ x → x + (n + 1)) (*-distribˡ-+ n n 1) ⟩
    n * n + n * 1 + (n + 1)
  ≡⟨ cong (λ x → n * n + x + (n + 1)) (*-identityʳ n) ⟩
    n * n + n + (n + 1)
  ≡⟨ +-assoc (n * n) n  (n + 1) ⟩
    n * n + (n + (n + 1))
  ≡⟨ cong (λ x → n * n + x) (sym (+-assoc n n 1)) ⟩
    n * n + ((n + n) + 1)
  ≡⟨ sym (+-assoc (n * n) (n + n) 1) ⟩
    n * n + (n + n) + 1
  ≡⟨ cong (λ x → n * n + (n + x) + 1) (+-comm 0 n) ⟩
    n * n + 2 * n + 1
  ≡⟨ cong (λ x → x * x + 2 * n + 1) (sym (^-identityʳ n)) ⟩
    n ^ 1 * n ^ 1 + 2 * n + 1
  ≡⟨ cong (λ x → x + 2 * n + 1) (sym (^-distribˡ-+-* n 1 1)) ⟩
    n ^ 2 + 2 * n + 1
  ∎