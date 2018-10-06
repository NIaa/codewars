module Fibonacci where
fib :: Integer -> Integer
fib n = (signum n) ^ (1 + abs n) * (pow n)!!1!!0 where
pow n
    | abs x == 0 = [[1,0],[0,1]]
    | abs x == 1 = [[0,1],[1,1]]
    | mod x 2 == 0 =  
        [[a!!0!!0^2+a!!0!!1*a!!1!!0,a!!0!!1*(a!!0!!0+a!!1!!1)],
        [a!!1!!0*(a!!0!!0+a!!1!!1),a!!1!!1^2+a!!0!!1*a!!1!!0]] 
    | otherwise = [[b!!0!!1,b!!0!!0+b!!0!!1],[b!!1!!1,b!!1!!0+b!!1!!1]]
    where
        x = abs n
        a = pow (div x 2)
        b = pow (x - 1)