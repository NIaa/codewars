module Haskell.Codewars.Church where
--import Haskell.Codewars.Church.Preloaded -- Remove before submit

type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 c2 f x = c1 f (c2 f x)

churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 f x = c1 (c2 f) x 

--Extra credit: Why is the type signature different?
churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce f x = (ce cb) f x