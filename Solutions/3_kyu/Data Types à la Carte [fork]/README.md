<https://www.codewars.com/kata/data-types-a-la-carte-fork/train/haskell>
<b><b>
```haskell
data Expr = Lit Int | Add Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

pretty :: Expr -> String
pretty (Lit n) = show n
pretty (Add e1 e2) = pretty e1 ++ "+" ++ pretty e2

data Expr = Lit Int | Add Expr Expr | Mult Expr Expr
```