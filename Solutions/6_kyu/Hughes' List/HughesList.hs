module HughesList where
-- "write only" "faster append"

import Data.Monoid

newtype Hughes a = Hughes ([a] -> [a])
-- no ++, no isomorphism to pass slowrev test

runHughes :: Hughes a -> [a]
runHughes (Hughes k) = k []

mkHughes :: [a] -> Hughes a
mkHughes l = Hughes (l ++)

------------------------------------------------------------

consDumb :: a -> Hughes a -> Hughes a
consDumb a h = mkHughes (a : runHughes h)

cons :: a -> Hughes a -> Hughes a
cons a (Hughes b) = Hughes ((a:) . b)

------------------------------------------------------------

appendDumb :: Hughes a -> Hughes a -> Hughes a
appendDumb a b = mkHughes (runHughes a ++ runHughes b)

append :: Hughes a -> Hughes a -> Hughes a
append (Hughes p) (Hughes q) = Hughes (p . q) 

--instance Semigroup (Hughes a) where
  --(<>) = append

instance Monoid (Hughes a) where
  mempty = mkHughes []
  mappend = append

------------------------------------------------------------

snocDumb :: Hughes a -> a -> Hughes a
snocDumb l a = mkHughes (runHughes l ++ [a])

snoc :: Hughes a -> a -> Hughes a
snoc (Hughes l) a = Hughes (l . (a:))

