{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kata.TimesComm where

import Kata.TimesComm.Definitions

infixl 6 |+|
(|+|) :: Natural a -> Natural b -> Natural (a :+: b)
NumZ |+| n = n
(NumS m) |+| n = NumS (m |+| n)

infixr 7 |*|
(|*|) :: Natural a -> Natural b -> Natural (a :*: b)
NumZ |*| m = NumZ
(NumS m) |*| n = n |+| (m |*| n)
------------------------------------------------------------------------------------------------
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

congPlusR :: Natural a -> Natural b -> Equal a b -> Natural c -> Equal (a :+: c) (b :+: c)
congPlusR m n eq NumZ = plusIdentityR m ~~ eq ~~ sym (plusIdentityR n)
congPlusR m n eq (NumS p) = sym (lemmaPlusSuccR m p) ~~ EqlS (congPlusR m n eq p) ~~ lemmaPlusSuccR n p

congPlusL :: Natural a -> Natural b -> Equal a b -> Natural c -> Equal (c :+: a) (c :+: b)
congPlusL m n eq p = plusComm p m ~~ congPlusR m n eq p ~~ plusComm n p
------------------------------------------------------------------------------------------------
plusIdentityR :: Natural a -> Equal (a :+: Z) a
plusIdentityR NumZ = EqlZ
plusIdentityR (NumS n) = EqlS $ plusIdentityR n

plusIdentityL :: Natural a -> Equal (Z :+: a) a
plusIdentityL = refl

multZeroR :: Natural a -> Equal (a :*: Z) Z
multZeroR NumZ = EqlZ
multZeroR (NumS n) = multZeroR n

multZeroL :: Natural a -> Equal (Z :*: a) Z
multZeroL NumZ = EqlZ
multZeroL (NumS n) = multZeroL n

plusAssoc :: Natural a -> Natural b -> Natural c -> Equal ((a :+: b) :+: c) (a :+: (b :+: c))
plusAssoc NumZ NumZ c = refl c
plusAssoc NumZ (NumS b) c = EqlS $ plusAssoc NumZ b c
plusAssoc (NumS a) b c = EqlS $ plusAssoc a b c

plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm NumZ NumZ = EqlZ
plusComm NumZ (NumS n) = EqlS $ plusComm NumZ n
plusComm (NumS m) n = EqlS (plusComm m n) ~~ lemmaPlusSuccR n m

multComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
multComm m NumZ = multZeroR m
multComm m (NumS n) = sym (lemmaMultSuccR m n) ~~ congPlusL (m |*| n) (n |*| m) (multComm m n) m
------------------------------------------------------------------------------------------------
lemmaPlusSuccR :: Natural a -> Natural b -> Equal (S a :+: b) (a :+: S b)
lemmaPlusSuccR NumZ n = EqlS $ refl n
lemmaPlusSuccR (NumS m) n = EqlS $ lemmaPlusSuccR m n

lemmaMultSuccR :: Natural a -> Natural b -> Equal (a :+: (a :*: b)) (a :*: S b)
lemmaMultSuccR NumZ n = plusIdentityL (NumZ |*| n) ~~ multZeroL n ~~ sym (multZeroL $ NumS n)
lemmaMultSuccR (NumS m) n = EqlS (lemmaPlusSwap m n (m |*| n))
    ~~ EqlS (congPlusL (m |+| m |*| n) (m |*| NumS n) (lemmaMultSuccR m n) n)

lemmaPlusSwap :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) (b :+: (a :+: c))
lemmaPlusSwap m n p = sym (plusAssoc m n p) ~~ congPlusR (m |+| n) (n |+| m) (plusComm m n) p
    ~~ plusAssoc n m p
------------------------------------------------------------------------------------------------
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm = multComm