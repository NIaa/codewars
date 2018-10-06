module ISO where
import Data.Void
import Data.Maybe

-- Please copy your code of Isomorphism to here.

type ISO a b = (a -> b, b -> a)
-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst
  
-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd
  
-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)
  
isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)
  
-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)
  
-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (a, b) = (b, a)
  
-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb) 
  
-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))
  
isoList :: ISO a b -> ISO [a] [b]
isoList (a, b) = ((a <$>), (b <$>))
  
isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (a, b) = ((a <$>), (b <$>))
  
isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (fun ab cd, fun ba dc)
  where fun f g ex = case ex of
          (Left  x) -> Left  $ f x
          (Right x) -> Right $ g x
  
isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = ((\ac -> cd . ac . ba), (\bd -> dc . bd . ab))

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (mamb, mbma) = (\a -> unJust $ mamb $ Just a, substL $ isoUnMaybe $ symm (mamb, mbma)) where
  unJust (Just a) = a
  unJust Nothing = unVar (mamb Nothing)
  -- determines whether the var is Nothing or not.
  unVar (Just a) = a
  unVar Nothing = undefined

-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.
 
-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
  
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (f, g) where
  f (Right ()) = Left []
  f (Left x ) = Left $ (): x
  g (Left []) = Right ()
  g( Left (_:xs)) = Left xs

-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither
  
-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)

-- Sometimes, we can treat a Type as a Number:
-- if a Type t has n distinct value, it's Number is n.
-- This is formally called cardinality.
-- See https://en.wikipedia.org/wiki/Cardinality

-- Void has cardinality of 0 (we will abbreviate it Void is 0).
-- () is 1.
-- Bool is 2.
-- Maybe a is 1 + a.
-- We will be using peano arithmetic so we will write it as S a.
-- https://en.wikipedia.org/wiki/Peano_axioms
-- Either a b is a + b.
-- (a, b) is a * b.
-- a -> b is b ^ a. Try counting (() -> Bool) and (Bool -> ())

-- Algebraic data type got the name because
-- it satisfies a lot of algebraic rules under isomorphism

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (f, f) where
  f (Left  x) = Right x
  f (Right x) = Left  x

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (left, right) where
  left (Left (Left a )) = Left a
  left (Left (Right b)) = Right $ Left  b
  left (Right c)        = Right $ Right c
  right (Left a) = Left (Left a)
  right (Right (Left  b)) = Left (Right b)
  right (Right (Right c)) = Right c

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (f, f) where f = \(x, y) -> (y, x)

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (\((a, b), c) -> (a, (b, c)), \(a, (b, c)) -> ((a, b), c))

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = (left, right) where
  left (a, Left  b) = Left  (a, b)
  left (a, Right c) = Right (a, c)
  right (Left  (a, b)) = (a, Left  b)
  right (Right (a, c)) = (a, Right c)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (\f -> \(a, b) -> f a b, \f -> \a b -> f(a, b))
-- saw it in Prelude

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (f, isJust) where
  f True  = Just Nothing
  f False = Nothing

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
  where
    left (Left  x) = absurd x -- absurd :: Void -> a
    left (Right x) = x
    
    
-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (left, right) where
  left (Left Nothing) = Nothing
  left (Left (Just a)) = Just $ Left a
  left (Right b) = Just (Right b)
  right Nothing = Left Nothing
  right (Just (Left  a)) = Left (Just a)
  right (Just (Right b)) = Right b

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (undefined, absurd)

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (left, right) where
  left (Nothing, b) = Left b
  left (Just a, b) = Right (a, b) 
  right (Left b) = (Nothing, b)
  right (Right (a, b)) = (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans` 
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (\_ -> (), \_ -> absurd)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (\f -> (f Nothing, f . Just), \(a, f) -> \b -> fromMaybe a $ f <$> b)
-- (Maybe b -> a) -> (a, b -> a)
-- (a, b -> a) -> Maybe b -> a

-- a ^ 1 = a
-- Go the hard way (like multSO, plusSO)
-- to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO = (\f -> f (), const)

-- Here's a trick: 
-- replace undefined with the rhs of the comment on previous line
-- When you're not sure what to fill in for a value,
-- Have it as a _
-- GHC will goes like
-- "Found hole `_' with type: ISO (() -> a) (Maybe b0 -> a0)"
-- So you can immediately see value of what type are needed
-- This process can be repeat indefinitely -
-- For example you might replace `_` with `isoFunc _ _`
-- So GHC hint you on more specific type.
-- This is especially usefull if you have complex type.
-- See https://wiki.haskell.org/GHC/Typed_holes
-- And "stepwise refinement" for more details.