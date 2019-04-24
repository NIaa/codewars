{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module NatProperties where
------------------------------------------------------------------------------------------------
-- | The natural numbers, encoded in types.
data Z
data S n

data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

infixl 4 ===
type a === b = Equal a b

-- | Peano definition of addition.
infixl 6 :+:
type family (:+:) (m :: *) (n :: *) :: *
type instance Z :+: n = n
type instance S m :+: n = S (m :+: n)

infixl 6 |+|
(|+|) :: Natural a -> Natural b -> Natural (a :+: b)
NumZ |+| n = n
(NumS m) |+| n = NumS (m |+| n)

-- | Peano definition of multiplication.
infixl 7 :*:
type family (:*:) (m :: *) (n :: *) :: *
type instance Z :*: n = Z
type instance S m :*: n = n :+: (m :*: n)

infixr 7 |*|
(|*|) :: Natural a -> Natural b -> Natural (a :*: b)
NumZ |*| m = NumZ
(NumS m) |*| n = n |+| (m |*| n)
------------------------------------------------------------------------------------------------
-- | Propositional Equality.
refl :: Natural n -> Equal n n
refl NumZ = EqlZ
refl (NumS n) = EqlS $ refl n

sym :: Equal a b -> Equal b a
sym EqlZ = EqlZ
sym (EqlS eq) = EqlS $ sym eq

trans :: Equal a b -> Equal b c -> Equal a c
trans EqlZ EqlZ = EqlZ
trans (EqlS eq1) (EqlS eq2) = EqlS $ trans eq1 eq2

infixl 4 ~~
(~~) = trans

-- | Weak type system, cannot implement a cong for all functions.
congPlusR :: Natural a -> Natural b -> a === b -> Natural c -> a :+: c === b :+: c
congPlusR m n eq NumZ = plusIdentityR m ~~ eq ~~ sym (plusIdentityR n)
congPlusR m n eq (NumS p) = sym (lemmaPlusSuccR m p) ~~ EqlS (congPlusR m n eq p) ~~ lemmaPlusSuccR n p

congPlusL :: Natural a -> Natural b -> a === b -> Natural c -> c :+: a === c :+: b
congPlusL m n eq p = plusComm p m ~~ congPlusR m n eq p ~~ plusComm n p
------------------------------------------------------------------------------------------------
-- | Natural numbers' properties.
plusIdentityR :: Natural a -> a :+: Z === a
plusIdentityR NumZ = EqlZ
plusIdentityR (NumS n) = EqlS $ plusIdentityR n

plusIdentityL :: Natural a -> Z :+: a === a
plusIdentityL = refl

plusAssoc :: Natural a -> Natural b -> Natural c -> a :+: b :+: c === a :+: (b :+: c)
plusAssoc NumZ NumZ c = refl c
plusAssoc NumZ (NumS b) c = EqlS $ plusAssoc NumZ b c
plusAssoc (NumS a) b c = EqlS $ plusAssoc a b c

plusComm :: Natural a -> Natural b -> a :+: b === b :+: a
plusComm NumZ NumZ = EqlZ
plusComm NumZ (NumS n) = EqlS $ plusComm NumZ n
plusComm (NumS m) n = EqlS (plusComm m n) ~~ lemmaPlusSuccR n m

multZeroL :: Natural a -> Z :*: a === Z
multZeroL NumZ = EqlZ
multZeroL (NumS n) = multZeroL n

multZeroR :: Natural a -> a :*: Z === Z
multZeroR NumZ = EqlZ
multZeroR (NumS n) = multZeroR n

multComm :: Natural a -> Natural b -> a :*: b === b :*: a
multComm m NumZ = multZeroR m
multComm m (NumS n) = sym (lemmaMultSuccR m n) ~~ congPlusL (m |*| n) (n |*| m) (multComm m n) m

multDistribPlusR :: Natural a -> Natural b -> Natural c -> (a :+: b) :*: c === a :*: c :+: b :*: c
multDistribPlusR NumZ n p = refl $ n |*| p
multDistribPlusR (NumS m) n p = congPlusL ((m |+| n) |*| p) (m |*| p |+| n |*| p) (multDistribPlusR m n p) p 
    ~~ sym (plusAssoc p (m |*| p) (n |*| p))

multAssoc :: Natural a -> Natural b -> Natural c -> a :*: b :*: c === a :*: (b :*: c)
multAssoc NumZ n p = EqlZ
multAssoc (NumS m) n p = multDistribPlusR n (m |*| n) p ~~ 
    congPlusL ((m |*| n) |*| p) (m |*| (n |*| p)) (multAssoc m n p) (n |*| p)
------------------------------------------------------------------------------------------------
-- | Lemmas.
lemmaPlusSuccR :: Natural a -> Natural b -> S a :+: b === a :+: S b
lemmaPlusSuccR NumZ n = EqlS $ refl n
lemmaPlusSuccR (NumS m) n = EqlS $ lemmaPlusSuccR m n

lemmaMultSuccR :: Natural a -> Natural b -> a :+: a :*: b === a :*: S b
lemmaMultSuccR NumZ n = plusIdentityL (NumZ |*| n) ~~ multZeroL n ~~ sym (multZeroL $ NumS n)
lemmaMultSuccR (NumS m) n = EqlS (lemmaPlusSwap m n (m |*| n))
    ~~ EqlS (congPlusL (m |+| m |*| n) (m |*| NumS n) (lemmaMultSuccR m n) n)

lemmaPlusSwap :: Natural a -> Natural b -> Natural c -> a :+: (b :+: c) === b :+: (a :+: c)
lemmaPlusSwap m n p = sym (plusAssoc m n p) ~~ congPlusR (m |+| n) (n |+| m) (plusComm m n) p
    ~~ plusAssoc n m p