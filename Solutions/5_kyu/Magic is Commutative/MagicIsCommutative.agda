{-# OPTIONS --safe #-}
module MagicIsCommutative where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong)

import Relation.Binary.EqReasoning as EqR

record IsMagical {A : Set} (_∙_ : A → A → A) : Set where
  field
    left         : ∀ x y → (x ∙ y) ∙ y  ≡  x
    right        : ∀ x y → y ∙ (y ∙ x)  ≡  x

record IsCommuntative {A : Set} (_∙_ : A → A → A) : Set where
  field
    comm         : ∀ x y → x ∙ y  ≡ y ∙ x

open IsMagical
open IsCommuntative

magic-is-commutative : {A : Set} (_∙_ : A → A → A) → IsMagical _∙_ → IsCommuntative _∙_
magic-is-commutative {A} _∙_ magic = record { comm = helper }  where
  helper : ∀ (x y : A) → x ∙ y ≡ y ∙ x
  helper x y rewrite cong (_∙ y) (sym (right magic x (y ∙ x))) | left magic y x | left magic (y ∙ x) y = refl
