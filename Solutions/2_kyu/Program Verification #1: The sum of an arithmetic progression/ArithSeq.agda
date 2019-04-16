{-# OPTIONS --safe #-}
module ArithSeq where

open import Data.Nat
open import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning
open import Data.Nat.Properties using (*-distribˡ-+; +-comm; +-assoc; *-identityʳ; +-identityʳ)
open import Preloaded

{-
module Preloaded where

open import Data.Nat

arith-sum : ℕ → ℕ
arith-sum zero = zero
arith-sum (suc n) = suc n + arith-sum n

arith-formula : ℕ → ℕ
arith-formula n = ⌊ n * (n + 1) /2⌋
-}


arith-eq : (n : ℕ) -> arith-formula n ≡ arith-sum n
arith-eq zero = refl
arith-eq (suc n) = arith-formula (suc n)        
  ≡⟨ refl ⟩ ⌊ suc (n + 1 + n * suc (n + 1)) /2⌋
  ≡⟨ cong (λ x → ⌊ suc (n + 1 + n * x) /2⌋) (+-comm 1 (n + 1))⟩ ⌊ suc (n + 1 + n * (n + 1 + 1)) /2⌋
  ≡⟨ cong (λ x → ⌊ suc (n + 1 + n * x) /2⌋) (+-assoc n 1 1) ⟩  ⌊ suc (n + 1 + n * (n + 2)) /2⌋
  ≡⟨ cong (λ x → ⌊ suc (n + 1 + x) /2⌋) (*-distribˡ-+ n n 2) ⟩ ⌊ suc (n + 1 + (n * n + n * 2)) /2⌋
  ≡⟨ cong (λ x → ⌊ suc (x + (n * n + n * 2)) /2⌋) (+-comm n 1) ⟩ suc ⌊ n + (n * n + n * 2) /2⌋
  ≡⟨ cong (λ x → suc ⌊ x /2⌋) (sym (+-assoc n (n * n) (n * 2))) ⟩ suc ⌊ n + n * n + n * 2 /2⌋
  ≡⟨ cong (λ x → suc ⌊ x + n * n + n * 2 /2⌋) (sym (*-identityʳ n)) ⟩ suc ⌊ n * 1 + n * n + n * 2 /2⌋
  ≡⟨ cong (λ x → suc ⌊ x + n * 2 /2⌋) (sym (*-distribˡ-+ n 1 n)) ⟩ suc ⌊ n * suc n + n * 2 /2⌋
  ≡⟨ cong suc (helper (n * suc n) n) ⟩ suc (n + ⌊ n * suc n /2⌋)
  ≡⟨ cong (λ x → suc (n + ⌊ n * x /2⌋)) (+-comm 1 n) ⟩ suc (n + ⌊ n * (n + 1) /2⌋)
  ≡⟨ cong (λ x → suc (n + x)) (arith-eq n) ⟩ suc (n + arith-sum n)
  ∎
  where
    helper : ∀ (x k : ℕ) →  ⌊ x + k * 2 /2⌋ ≡ k + ⌊ x /2⌋
    helper x zero rewrite +-identityʳ x = refl
    helper x (suc k) rewrite +-comm x (suc (suc (k * 2))) | +-comm (k * 2) x | helper x k = refl
