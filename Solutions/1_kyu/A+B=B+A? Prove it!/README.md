<https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/train/haskell>  
<b><b>
```haskell
data Z
data S n

data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a) 
```