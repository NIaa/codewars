{-# OPTIONS --safe #-}

module AdHoc where

open import Data.Char
open import Data.String hiding (length)
open import Data.List
open import Data.Integer as I
open import Data.Nat as N
open import Agda.Builtin.Nat using (_==_)
open import Data.Bool as B
open import Agda.Builtin.Char
open import Relation.Nullary.Decidable

record Eq {a} (A : Set a) : Set a where
  field
    _≣_ : A → A → Bool

open Eq ⦃...⦄ public

_===_ : {A : Set} → ⦃ _ : Eq A ⦄ → A → A → Bool
_=/=_ : {A : Set} → ⦃ _ : Eq A ⦄ → A → A → Bool
_===_ = _≣_
x =/= y = not (x ≣ y)


-- ℤ, ℕ, Char, Bool, String, Eq e => List e will be tested.
-- try both definition and copattern
instance
  eqℤ : Eq ℤ
  eqℤ = record {_≣_ = λ x x₁ → ⌊ x I.≟ x₁ ⌋}

  eqℕ : Eq ℕ
  eqℕ = record {_≣_ = _==_}

  eqChar : Eq Char
  eqChar = record {_≣_ = primCharEquality}

  eqBool : Eq Bool
  _≣_ ⦃ eqBool ⦄ false false = true
  _≣_ ⦃ eqBool ⦄ false true  = false
  _≣_ ⦃ eqBool ⦄ true  false = false
  _≣_ ⦃ eqBool ⦄ true  true  = true

  eqString : Eq String
  eqString = record { _≣_ = primStringEquality}

  eqList : {A : Set} ⦃ _ : Eq A ⦄ → Eq (List A)
  _≣_ ⦃ eqList ⦄ [] [] = true
  _≣_ ⦃ eqList ⦄ [] (x ∷ l2) = false
  _≣_ ⦃ eqList ⦄ (x ∷ l1) [] = false
  _≣_ ⦃ eqList ⦄ (x ∷ l1) (x₁ ∷ l2) = _≣_ ⦃ eqList ⦄ l1 l2
  
  