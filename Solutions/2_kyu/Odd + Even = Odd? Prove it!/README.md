<https://www.codewars.com/kata/odd-plus-even-equals-odd-prove-it/train/haskell>
<b><b>
```haskell
-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of the even numbers.
data Even (n :: Nat) where
  -- | Axiom: zero is even.
  ZeroEven :: Even Z
  -- | Axiom: if n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of the odd numbers.
data Odd (n :: Nat) where
  -- | Axiom: one is odd.
  OneOdd :: Odd (S Z)
  -- | Axiom: if n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

evenPlusOdd :: Even n -> Odd m -> Odd (Add m n)
evenPlusOdd = -- (proof here)  
```