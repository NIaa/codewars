{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

data State s a = State {runState :: s -> (a, s)}
instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k = State $ \s -> let (a, s') = runState m s in runState (k a) s'

data Reader s a = Reader {runReader :: s -> a }
instance Monad (Reader r) where
  return a = Reader $ \_ -> a
  m >>= k  = Reader $ \r -> runReader(k (runReader m r)) r

data Writer w a = Writer {runWriter :: (w, a)}
instance Monoid w => Monad (Writer w) where
  return val = Writer (mempty, val)
  (Writer (log, val)) >>= f = Writer(log `mappend` log1, val1) where Writer(log1, val1) = f val