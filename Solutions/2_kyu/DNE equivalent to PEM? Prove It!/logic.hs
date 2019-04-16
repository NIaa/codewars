{-# LANGUAGE RankNTypes #-}

module Kata where

import Prelude hiding (undefined, error)

import KataPreloaded

{- Preloaded code :
type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a
-}

from :: AxiomDNE -> AxiomPEM
from av_v_a = \ab av_b -> av_v_a $ \bv -> bv $ av_b $ bv . ab

to :: AxiomPEM -> AxiomDNE
to pem = \av_v -> pem id $ absurd . av_v