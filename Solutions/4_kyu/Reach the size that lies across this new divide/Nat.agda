{-# OPTIONS --safe --sized-types #-}
module Nat where

--open import Agda.Builtin.Nat public
open import Agda.Builtin.Size public

data Nat : {i : Size} -> Set where
  zero : {i : Size} -> Nat {↑ i}
  suc :  {i : Size} -> Nat {i} -> Nat{↑ i}

SubType = {i : Size} -> Nat {i} -> Nat {∞} -> Nat {i} 
DivType = {i : Size} -> Nat {i} -> Nat -> Nat {i}

-- https://agda.readthedocs.io/en/latest/language/sized-types.html?highlight=sized%20types
-- http://www.cse.chalmers.se/~abela/talkAIM2008Sendai.pdf