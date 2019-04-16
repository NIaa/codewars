<https://www.codewars.com/kata/im-now-360-degree-different-from-the-past/train/agda>
<br>
Still not familiar with stl.  
@monadius 's solution looks better.  

```agda
{-# OPTIONS --safe #-}
module RevRev where

open import Relation.Binary.PropositionalEquality
open import Data.List
open import Data.List.Properties
open import Rev

lemma : ∀ {ℓ} {A : Set ℓ} (a b : List A) -> rev (a ++ b) ≡ rev b ++ rev a
lemma [] b = sym (++-identityʳ (rev b))
lemma (x ∷ a) b rewrite lemma a b = ++-assoc (rev b) _ _

revrevid : ∀ {ℓ} {A : Set ℓ} (a : List A) → rev (rev a) ≡ a
revrevid [] = refl
revrevid (x ∷ a) rewrite lemma (rev a) (x ∷ []) = cong (x ∷_) (revrevid a)
------- i. e.  
revrevid (x ∷ a) with ? | eqn = cong (x ∷_) (revrevid a)
...                | ._ | lemma (rev a) (x :: [])
```