module Haskell.Codewars.Peano where
    import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)
    
    data Peano = Zero | Succ Peano deriving (Eq, Show)
    
    add, sub, mul, div :: Peano -> Peano -> Peano
    -- Addition
    add Zero m = m
    add (Succ n) m = Succ (add n m)
    -- Subtraction
    sub n Zero = n
    sub Zero m = error "negative number"
    sub (Succ n) (Succ m) = sub n m
    -- Multiplication
    mul Zero m = Zero
    mul (Succ n) m = add (mul n m) m
    -- Integer division
    div n m
      | m==Zero = error "divide by 0" 
      | compare n m == LT = Zero
      | otherwise = add (Succ Zero) (div (sub n m) m)
    
    even, odd :: Peano -> Bool
    -- Even
    even Zero = True
    even (Succ n) = not (even n)
    -- Odd
    odd Zero = False
    odd (Succ n) = not (odd n)
    
    compare :: Peano -> Peano -> Ordering
    -- Compare
    compare Zero Zero = EQ
    compare n Zero = GT
    compare Zero m = LT
    compare (Succ n) (Succ m) = compare n m
    module Haskell.Codewars.Peano where
    import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)
    
    data Peano = Zero | Succ Peano deriving (Eq, Show)
    
    add, sub, mul, div :: Peano -> Peano -> Peano
    -- Addition
    add Zero m = m
    add (Succ n) m = Succ (add n m)
    -- Subtraction
    sub n Zero = n
    sub Zero m = error "negative number"
    sub (Succ n) (Succ m) = sub n m
    -- Multiplication
    mul Zero m = Zero
    mul (Succ n) m = add (mul n m) m
    -- Integer division
    div n m
      | m==Zero = error "divide by 0" 
      | compare n m == LT = Zero
      | otherwise = Succ $ div (sub n m) m
    
    even, odd :: Peano -> Bool
    -- Even
    even Zero = True
    even (Succ n) = not (even n)
    -- Odd
    odd Zero = False
    odd (Succ n) = not (odd n)
    
    compare :: Peano -> Peano -> Ordering
    -- Compare
    compare Zero Zero = EQ
    compare n Zero = GT
    compare Zero m = LT
    compare (Succ n) (Succ m) = compare n m