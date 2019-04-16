{-# OPTIONS --safe #-}
module Iso-properties where

open import Data.Nat.Properties using (+-comm)

open import Iso
open _⇔_

-- Task 0 : Example of _⇔_ in finite sets
-- Task 0-1. Find a bijection between Bool and Bit. (provided for you as an example)
data Bool : Set where
  true false : Bool
  
data Bit : Set where
  0b 1b : Bit

Bool→Bit : Bool → Bit
Bool→Bit false = 0b
Bool→Bit true = 1b

Bit→Bool : Bit → Bool
Bit→Bool 0b = false
Bit→Bool 1b = true

Bool→Bit→Bool : ∀ (b : Bool) → Bit→Bool (Bool→Bit b) ≡ b
Bool→Bit→Bool true = refl
Bool→Bit→Bool false = refl

Bit→Bool→Bit : ∀ (b : Bit) → Bool→Bit (Bit→Bool b) ≡ b
Bit→Bool→Bit 0b = refl
Bit→Bool→Bit 1b = refl

Bool⇔Bit : Bool ⇔ Bit
Bool⇔Bit = Bijection Bool→Bit Bit→Bool Bool→Bit→Bool Bit→Bool→Bit

--------------------------------------------------------------------
-- Task 1 : General properties of ⇔
-- Task 1-1. Prove that any set has the same cardinality as itself.
⇔-refl : ∀ {A : Set} → A ⇔ A
⇔-refl = Bijection (λ z → z) (λ z → z) (λ a → refl) (λ b → refl)

-- Task 1-2. Prove that _⇔_ relation is symmetric.
⇔-sym : ∀ {A B : Set} → A ⇔ B → B ⇔ A
⇔-sym (Bijection A→B B→A A→B→A B→A→B) = Bijection B→A A→B B→A→B A→B→A

-- Task 1-3. Prove that _⇔_ relation is transitive.
⇔-trans : ∀ {A B C : Set} → A ⇔ B → B ⇔ C → A ⇔ C
⇔-trans {A} {B} {C} (Bijection A→B B→A A→B→A B→A→B) (Bijection B→C C→B B→C→B C→B→C) =
  Bijection (λ z → B→C (A→B z)) (λ z → B→A (C→B z)) (λ a → helper1) λ c → helper2 where
  helper1 : ∀ {a : A} → B→A (C→B (B→C (A→B a))) ≡ a
  helper1 {a} rewrite cong B→A (B→C→B (A→B a)) = A→B→A a
  helper2 : ∀ {c : C} → B→C (A→B (B→A (C→B c))) ≡ c
  helper2 {c} rewrite cong B→C (B→A→B (C→B c)) = C→B→C c

-- Task 1-4. Prove the following statement:
--   Given two functions A→B and B→A, if A→B→A is satisfied and B→A is injective, A ⇔ B.
bijection-alt :
  ∀ {A B : Set} →
  (A→B : A → B) →
  (B→A : B → A) →
  (∀ a → B→A (A→B a) ≡ a) →
  (∀ b b' → B→A b ≡ B→A b' → b ≡ b') →
  A ⇔ B
bijection-alt A→B B→A A→B→A B→A-inj =
  Bijection A→B B→A A→B→A λ b → B→A-inj (A→B (B→A b)) b (A→B→A (B→A b))

--------------------------------------------------------------------
-- Task 2 : ⇔-relations between ℕ and various supersets of ℕ

-- ℕ+1 : A set having one more element than ℕ.
{- Preloaded code
data ℕ+1 : Set where
  null : ℕ+1
  nat : ℕ → ℕ+1
-}

-- Task 2-1. Prove that ℕ has the same cardinality as ℕ+1.
ℕ⇔ℕ+1 : ℕ ⇔ ℕ+1
ℕ⇔ℕ+1 = Bijection to from from∘to to∘from where
  to : ℕ → ℕ+1
  to zero = null
  to (suc n) = nat n
  from : ℕ+1 → ℕ
  from null = zero
  from (nat n+1) = suc n+1
  from∘to : ∀ (n : ℕ) → from (to n) ≡ n
  from∘to zero = refl
  from∘to (suc n) = refl
  to∘from : ∀ (n+1 : ℕ+1) → to (from n+1) ≡ n+1
  to∘from null = refl
  to∘from (nat x) = refl

-- ℕ+ℕ : A set having size(ℕ) more elements than ℕ.
{- Preloaded code
data ℕ+ℕ : Set where
  left : ℕ → ℕ+ℕ
  right : ℕ → ℕ+ℕ
-}

suc-ℕ+ℕ : ∀ (nn : ℕ+ℕ) → ℕ+ℕ
suc-ℕ+ℕ (left x) =  right x
suc-ℕ+ℕ (right x) = left (suc x)

-- Task 2-2. Prove that ℕ has the same cardinality as ℕ+ℕ.
ℕ⇔ℕ+ℕ : ℕ ⇔ ℕ+ℕ
ℕ⇔ℕ+ℕ = Bijection to from from∘to to∘from where
  to : ℕ → ℕ+ℕ
  to zero = left zero
  to (suc n) with to n
  ... | nn = suc-ℕ+ℕ nn
  from : ℕ+ℕ → ℕ
  from (left  l) = l + l
  from (right r) = suc (r + r)
  from∘to : ∀ (n : ℕ) → from (to n) ≡ n
  from∘to zero = refl
  from∘to (suc n) with to n | from∘to n
  ... | left l   | eql                          = cong suc eql
  ... | right r  | eql rewrite +-comm r (suc r) = cong suc eql
  to∘from : ∀ (nn : ℕ+ℕ) → to (from nn) ≡ nn
  to∘from (left zero) = refl
  to∘from (left (suc l)) with to∘from (right l)
  ... | eql rewrite +-comm l (suc l) = cong suc-ℕ+ℕ eql
  to∘from (right zero) = refl
  to∘from (right (suc r)) with to∘from (right r)
  ... | eql rewrite +-comm r (suc r) = cong (λ x → suc-ℕ+ℕ (suc-ℕ+ℕ x)) eql