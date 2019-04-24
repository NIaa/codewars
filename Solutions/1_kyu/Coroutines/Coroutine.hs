module Coroutine where

import Control.Monad (ap, forever, when, replicateM_)
import Preloaded

-- Preloaded contains the following:
-- {-# LANGUAGE DeriveFunctor #-}
--
-- newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)
--
-- data Command r u d a =
--   Done a
-- | Out d (Coroutine r u d a)
-- | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return a = Coroutine ($ Done a)
  Coroutine p >>= f = Coroutine $ \k -> p $ \cmd -> case cmd of
    Done a -> apply (f a) k
    Out d cr -> k $ Out d $ cr >>= f
    In u_cr -> k $ In $ \u -> u_cr u >>= f

-- p2 has baton
(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
cr1 >>> Coroutine p2 = Coroutine $ \k -> p2 $ \cmd -> case cmd of
  Done a -> k $ Done a
  Out d cr2 ->  k $ Out d $ cr1 >>> cr2
  -- p2 holds baton
  In u_cr2 -> apply (cr1 >>>== u_cr2) k
  -- passes baton to cr1

pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
pipe2 = (>>>==)

-- p has baton
(>>>==) :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
Coroutine p >>>== f = Coroutine $ \k -> p $ \cmd -> case cmd of
  Done a -> k $ Done a
  Out d cr -> apply (cr >>> f d) k
  -- pass baton to f
  In u_cr -> k $ In $ \u -> u_cr u >>>== f
  -- p holds baton

-- Library functions

-- Immediately output the argument.
output :: d -> Coroutine r u d ()
output d = Coroutine $ ($ Out d $ pure ())
-- output d = Coroutine $ \k -> k $ Out d $ pure ()

-- Waits for an input before returning it.
input :: Coroutine r a d a
input = Coroutine ($ In pure)
-- input = Coroutine $ \k -> k $ In pure

-- Output each element in a list in order.
produce :: [d] -> Coroutine r u d ()
produce = mapM_ output
-- produce [] = pure ()
-- produce (d:ds) = output d >>= const (produce ds)

-- Collect all outputted values into a list.
consume :: Coroutine [t] u t a -> [t]
consume (Coroutine p) = p $ \cmd -> case cmd of
  Out t cr -> t : consume cr
  _ -> []

-- Repeatedly request for input and output it, if it matches a predicate.
filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = forever $ do
  v <- input
  when (p v) $ output v
-- filterC p = forever $ input >>= \v -> when (p v) (output v)

-- Allow n items to pass through before terminating (similar to take from the prelude).
limit :: Int -> Coroutine r v v ()
limit n = replicateM_ n $ input >>= output

-- Disallow the first n items to pass through (similar to drop from the prelude).
suppress :: Int -> Coroutine r v v ()
suppress n = replicateM_ n input >> forever (input >>= output)

-- Repeatedly take two inputs and output their sum.
add :: Coroutine r Int Int ()
add = forever $ do
  m <- input
  n <- input
  output $ m + n
-- add = forever $ input >>= \m -> input >>= \n -> return $ m + n

-- Repeatedly receive input and output it twice.
duplicate :: Coroutine r v v ()
duplicate = forever $ do
  n <- input
  replicateM_ 2 $ output n
-- duplicate = forever $ input >>= \n -> replicateM_ 2 $ return n

-- Programs
p1, p2, p3, p4 :: Coroutine r Int Int ()

-- 1. A program which outputs the first 5 even numbers of a stream.
p1 = filterC even >>> limit 5

-- 2. A program which produces a stream of the triangle numbers 
p2 = produce $ (\n -> n * (n + 1) `div` 2) <$> [1..]

-- 3. A program which multiplies a stream by 2
p3 = duplicate >>> add

-- 4. A program which sums adjacent pairs of integers
p4 = duplicate >>> suppress 1 >>> add